#' Execute multiple R, R Markdown, and Quarto scripts
#'
#' This function executes multiple R scripts (.R), renders R Markdown documents (.Rmd),
#' and Quarto documents (.qmd). It can process individual files or all supported files
#' in a specified folder, with options for parallel execution.
#'
#' @param scripts A character vector of file paths to R, R Markdown, or Quarto scripts.
#'   If NULL, the function will look for scripts in the specified folder.
#' @param folder A character string specifying the path to a folder containing scripts
#'   to execute. If provided, all .R, .Rmd, and .qmd files in this folder will be processed.
#' @param parallel Logical; if TRUE, scripts will be executed in parallel. Default is FALSE.
#' @param num_cores Integer specifying the number of cores to use for parallel execution.
#'   If NULL (default), it will use one less than the total number of available cores.
#' @param ... Other arguments passed on to the run_script function,
#'
#' @return A list containing the execution results for each script. Each element of the
#'   list is a character string indicating the success or failure of the script execution.
#'
#' @importFrom rmarkdown render
#' @importFrom quarto quarto_render
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ
#'
#' @export
execute_scripts <- function(scripts = NULL,
                            folder = NULL,
                            parallel = FALSE,
                            num_cores = NULL,
                            summary_dir = getwd(),
                            ...) {
  if (!is.null(folder)) {
    # If a folder is specified, get all supported script files in the folder
    script_files <- list.files(path = folder,
                               pattern = "\\.(R|Rmd|qmd)$",
                               full.names = TRUE)
  } else if (is.character(scripts)) {
    # If scripts is a character vector, assume it's a list of file paths
    script_files <- scripts
  } else {
    stop("Invalid input. Please provide either a vector of script paths or a folder path.")
  }

  # Function to execute a single script
  execute_single_script <- function(script) {
    cat("Executing script:", script, "\n")

    result <- tryCatch({
      # source(script, local = TRUE)
      output <- run_script(script, ...)
      dplyr::tibble(
        script = normalizePath(script),
        status = output$status$status,
        location = normalizePath(output$log_details$location)
      )
    }, error = function(e) {
      paste("Error executing script:",
            script,
            "\nError message:",
            conditionMessage(e))
    })

    return(result)
  }

  if (parallel) {
    # Parallel execution with progress display
    if (is.null(num_cores)) {
      num_cores <- min(detectCores() - 1, 8)  # Use one less than the total number of cores
    }

    cat("Executing scripts in parallel using", num_cores, "cores\n")

    cl <- makeCluster(num_cores)

    # Define a function to execute a single script with progress display
    execute_single_script_with_progress <- function(script) {
      cat("Executing script:", script, "\n")  # Display progress
      execute_single_script(script)  # Execute the script
    }

    results <- parLapply(cl, script_files, execute_single_script_with_progress)
    stopCluster(cl)

  } else {
    # Sequential execution
    results <- lapply(script_files, execute_single_script)
  }

  # After obtaining the results, create a summary data frame
  summary_df <- dplyr::bind_rows(results) |>
    dplyr::mutate(
      status = factor(.data[["status"]], levels = c("error", "warning", "success"))
    ) |>
    dplyr::arrange(factor(.data[["status"]]))

  summary_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/summary.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  summary_log_html <- withr::local_tempfile(fileext = ".html")

  if (summary_dir == getwd()){
    summary_dir_f <- here::here()
  } else {
    summary_dir_f <- normalizePath(summary_dir)
  }

  withr::with_dir(
    tempdir(),
    render(
      input = summary_qmd,
      output_format = "html_document",
      output_file = summary_log_html,
      params = list(summary_df = summary_df, summary_dir = summary_dir_f),
      quiet = FALSE
    )
  )

  # Create requested outputs

  # if ("html" %in% out_formats) {
  file_copy <- tryCatch(
    file.copy(
      from = summary_log_html,
      to = file.path(summary_dir, "summary.html"),
      overwrite = TRUE
    )
  )

  return(summary_df)
}

#' @noRd
knit_print.whirl_summary_info <- function(x, path_rel_start, ...) {
  hold <- x |>
    data.frame(check.names = FALSE)

  row.names(hold) <- NULL
  ncols <- ncol(hold)

  hold <- hold |>
    dplyr::mutate(
      formated = file.path(
        fs::path_rel(.data[["location"]], start = path_rel_start)
      )
    )

  hold$location <- paste0(sprintf('<a href="%s" target="_blank">%s</a>', hold$formated, hold$location))

  hold <- hold |>
    dplyr::select(-.data[["formated"]])

  knitr::kable(hold, format = "html", escape = FALSE) |>
    kableExtra::column_spec(1:ncols, background = ifelse(
      hold[["status"]] == "error",
      "#fceeef",
      ifelse(
        hold[["status"]]  == "warning",
        "#fffaea",
        ifelse(hold[["status"]]  == "success", "#ebf5f1", "white")
      )
    )) |>
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) |>
    knitr::knit_print()
}
