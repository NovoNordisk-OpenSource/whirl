#' Execute multiple R, R Markdown, and Quarto scripts
#'
#' This function executes multiple R scripts (.R), renders R Markdown documents (.Rmd),
#' and Quarto documents (.qmd). It can process individual files or all supported files
#' in a specified folder, with options for parallel execution.
#'
#' @param paths  A character vector of file paths to R, R Markdown, or Quarto scripts.
#'   If NULL, the function will look for scripts in the specified folder.
#' @param parallel Logical; if TRUE, scripts will be executed in parallel. Default is FALSE.
#' @param num_cores Integer specifying the number of cores to use for parallel execution.
#'   If NULL (default), it will use one less than the total number of available cores.
#' @param ... Other arguments passed on to the [run_script] function,
#' @param summary_dir A character string of file path specifying the directory where the summary log will be stored.
#'
#' @return A list containing the execution results for each script. Each element of the
#'   list is a character string indicating the success or failure of the script execution.
#'
#' @importFrom rmarkdown render
#' @importFrom quarto quarto_render
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ
#' @export
log_scripts <- function(paths  = NULL,
                        parallel = FALSE,
                        num_cores = NULL,
                        summary_dir = getwd(),
                        ...) {
  if (!is.null(num_cores) &&
      (!is.numeric(num_cores) || num_cores <= 0)) {
    stop("Invalid input for 'num_cores'. Please provide a positive numeric value.")
  }

  if (!is.null(summary_dir) && !dir.exists(summary_dir)) {
    stop("The specified summary directory does not exist.")
  }

  script_files <- character(0)  # Initialize all_files as an empty character vector

  for (path in paths) {
    if (file.exists(path)) {
      if (file.info(path)$isdir) {
        # If input is a directory, list files with specific extensions
        files <- list.files(path = path, recursive = TRUE, pattern = "\\.(R|Rmd|qmd)$", ignore.case = TRUE, full.names = TRUE)
        script_files <- c(script_files, files)
      } else {
        # If input is a file, add it directly to the list
        script_files <- c(script_files, path)
      }
    } else {
      cli::cat_line("File or folder does not exist: ", path)
    }
  }

  script_files <- unique(script_files)

  # else {
  #   stop("Missing input. Please provide either a vector of script paths or a folder path.")
  # }

  if (parallel) {
    # Parallel execution with progress display
    if (is.null(num_cores)) {
      num_cores <- min(detectCores() - 1, 8)  # Use one less than the total number of cores
    }

    cli::cli_inform("Executing scripts in parallel using {num_cores} cores\n")

    cl <- parallel::makeCluster(num_cores)

    results <- parallel::parLapply(cl, script_files, execute_single_script, ...)
    parallel::stopCluster(cl)

  } else {
    # Sequential execution
    results <- lapply(script_files, execute_single_script, ...)
  }

  # After obtaining the results, create a summary data frame

  summary_df <- dplyr::bind_rows(results) |>
    dplyr::mutate(Status = factor(.data[["Status"]], levels = c("error", "warning", "success", "skip"))) |>
    dplyr::arrange(factor(.data[["Status"]]))

  summary_qmd <- withr::local_tempfile(lines = readLines(system.file("documents/summary.qmd", package = "whirl")), fileext = ".qmd")

  summary_log_html <- withr::local_tempfile(fileext = ".html")

  if (summary_dir == getwd()) {
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
      params = list(summary_df = summary_df, summary_dir = summary_dir_f),
      quiet = TRUE
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

# Function to execute a single script
#' @noRd
execute_single_script <- function(script, ...) {
  not_null <- function(x){
    if(length(x) == 0 | is.null(x)){
      return("")
    }

    x_max <- x[1:4]
    msg_ <- paste(x_max[!is.na(x_max)], collapse = "<br>")

    if(length(x) > 4){
      msg_ <- paste(
        msg_,
        "<br>",
        "... See HTLM logs for more details",
        collapse = ""
      )
    }
    return(msg_)

  }

  result <- tryCatch({

    cli::cli_alert_info(script)

    if(!whirl_file_exits()){
      output <- run_script(script, ...)
    }else{
      output <- list(
        log_details = list(
          location = ""
        ),
        status = list(
          status = "skip",
          skip = "Skip because of previous errors"
        )
      )
    }

    tibble::tibble(
      Directory = dirname(normalizePath(script, winslash = "/")),
      Filename = basename(normalizePath(script, winslash = "/")),
      Status = output$status$status,
      Hyperlink = if(output$log_details$location == ""){""}else{normalizePath(output$log_details$location, winslash = "/")},
      Information = ""
    ) |>
      dplyr::mutate(Information = dplyr::case_when(
        output$status$status == "error" ~ not_null(output$status$error),
        output$status$status == "warning" ~ not_null(output$status$warning),
        output$status$status == "success" ~ not_null(output$status$success),
        output$status$status == "skip" ~ not_null(output$status$skip),
        TRUE ~ ""
      )
      )
  }, error = function(e) {
    tibble::tibble(
      Directory = dirname(normalizePath(script, winslash = "/")),
      Filename = basename(normalizePath(script, winslash = "/")),
      Status = "error",
      Hyperlink = NA_character_,
      Information = conditionMessage(e)
    )
  })

  return(result)
}


#' @noRd
knit_print.whirl_summary_info <- function(x, path_rel_start, ...) {
  hold <- x |>
    data.frame(check.names = FALSE)

  row.names(hold) <- NULL
  ncols <- ncol(hold)

  if (grepl("rstudio_cloud", Sys.getenv("R_CONFIG_ACTIVE"))) {
    hold <- hold |> dplyr::mutate(formated = file.path("/file_show?path=", .data[["Hyperlink"]]))
  }else{
    hold <- hold |> dplyr::mutate(formated = file.path(fs::path_rel(.data[["Hyperlink"]], start = path_rel_start)))
  }
  #
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
        ifelse(hold[["Status"]]  == "success", "#ebf5f1",
               ifelse(hold[["Status"]]  == "skip", "#94CBFF", "white")
        )
      )
    )
    ) |>
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) |>
    knitr::knit_print()
}
