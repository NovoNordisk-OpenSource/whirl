#' Execute multiple R, R Markdown, and Quarto scripts
#'
#' This function executes multiple R scripts (.R), renders R Markdown documents (.Rmd),
#' and Quarto documents (.qmd). It can process individual files or all supported files
#' in a specified folder, with options for parallel execution.
#'
#' @noRd
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
        Directory = dirname(normalizePath(script, winslash = "/")),
        Filename = basename(normalizePath(script, winslash = "/")),
        Status = output$status$status,
        Hyperlink = normalizePath(output$log_details$location, winslash = "/")
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
      Status = factor(.data[["Status"]], levels = c("error", "warning", "success"))
    ) |>
    dplyr::arrange(factor(.data[["Status"]]))

  summary_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/summary.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  summary_log_html <- withr::local_tempfile(fileext = ".html")

  if (summary_dir == getwd()){
    summary_dir_f <- here::here()
  } else {
    summary_dir_f <- normalizePath(summary_dir, winslash = "/")
  }

  withr::with_dir(
    tempdir(),
    rmarkdown::render(
      input = summary_qmd,
      output_format = "html_document",
      output_file = summary_log_html,
      params = list(summary_df = summary_df, summary_dir = summary_dir_f)
    )
  )


  # Create requested outputs

  file_copy <- tryCatch(
    file.copy(
      from = summary_log_html,
      to = file.path(summary_dir, "summary.html"),
      overwrite = TRUE
    )
  )

  return(invisible(summary_df))
}

#' @noRd
knit_print.whirl_summary_info <- function(x, path_rel_start, ...) {
  hold <- x |>
    data.frame(check.names = FALSE)

  row.names(hold) <- NULL
  ncols <- ncol(hold)

  hold <- hold |>
    dplyr::mutate(formated = ifelse(
      grepl("posit.cloud", Sys.getenv("RS_SERVER_URL")),
      file.path("/file_show?path=", .data[["Hyperlink"]]),
      file.path(fs::path_rel(.data[["Hyperlink"]], start = path_rel_start))
    ))

  hold$Hyperlink <- paste0(sprintf('<a href="%s" target="_blank">%s</a>', hold$formated, "HTML Log"))

  hold <- hold |>
    dplyr::select(-.data[["formated"]])

  knitr::kable(hold, format = "html", escape = FALSE) |>
    kableExtra::column_spec(1:ncols, background = ifelse(
      hold[["Status"]] == "error",
      "#fceeef",
      ifelse(
        hold[["Status"]]  == "warning",
        "#fffaea",
        ifelse(hold[["Status"]]  == "success", "#ebf5f1", "white")
      )
    )) |>
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) |>
    knitr::knit_print()
}
