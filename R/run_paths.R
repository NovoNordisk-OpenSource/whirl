#' Execute multiple R, R Markdown, and Quarto scripts
#'
#' This function executes multiple R scripts (.R), renders R Markdown documents
#' (.Rmd), and Quarto documents (.qmd). It can process individual files or all
#' supported files in a specified folder, with options for parallel execution.
#'
#' @param paths  A character vector of file paths to R, R Markdown, or Quarto
#'   scripts or a folder.
#' @param parallel Logical; if TRUE, scripts will be executed in parallel.
#'   Default is FALSE.
#' @param num_cores Integer specifying the number of cores to use for parallel
#'   execution. If NULL (default), it will use one less than the total number of
#'   available cores.
#' @param summary_dir A character string of file path specifying the directory
#'   where the summary log will be stored.
#' @param summary \code{logical} If \code{TRUE} (default) then a summary.html is
#'   generated. If \code{FALSE} then the summary generation will be skipped.
#' @param ... Additional arguments
#'
#' @return A list containing the execution results for each script. Each element
#'   of the list is a character string indicating the success or failure of the
#'   script execution.
#'
#' @importFrom rmarkdown render
#' @importFrom quarto quarto_render
#' @importFrom parallel detectCores makeCluster stopCluster parLapply
#'   clusterEvalQ
#' @export
run_paths <- function(paths = ".",
                      parallel = FALSE,
                      num_cores = NULL,
                      summary_dir = getwd(),
                      summary = TRUE,
                      ...) {

  if (!is.character(paths)) {
    stop("Missing valid path input. Path must be a character.")
  }

  if (!is.null(num_cores) &&
    (!is.numeric(num_cores) || num_cores <= 0)) {
    stop("Invalid input for 'num_cores'. Please provide a positive numeric value.")
  }

  if (!is.null(summary_dir) && !dir.exists(summary_dir)) {
    stop("The specified summary directory does not exist.")
  }

  script_files <- character(0)

  for (path in paths) {
    if (file.exists(path)) {
      if (file.info(path)$isdir) {
        # If input is a directory, list files with specific extensions
        files <- list.files(
          path = path,
          recursive = TRUE,
          pattern = "\\.(R|Rmd|qmd)$",
          ignore.case = TRUE,
          full.names = TRUE
        )
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

  # Only initiate parallel if there are more than one script to execute
  if (parallel & length(script_files) > 1) {
    # Parallel execution with progress display
    if (is.null(num_cores)) {
      # Use one less than the total number of cores
      num_cores <- min(c(detectCores() - 1, 8, length(script_files)))
    }

    cli::cli_inform("Executing {length(script_files)} scripts in parallel using {num_cores} cores\n")

    #Future
    oplan <- future::plan(future::multisession,
                          workers = num_cores,
                          earlySignal = FALSE)

    on.exit(future::plan(oplan), add = TRUE)

    results <- future.apply::future_lapply(X = script_files,
                                FUN = execute_single_script,
                                future.seed = 1)

  } else {
    # Sequential execution
    results <- lapply(script_files, execute_single_script,  verbose = FALSE, ...)
  }

  # After obtaining the results, create a summary data frame

  summary_df <- dplyr::bind_rows(results) |>
    dplyr::mutate(Status = factor(.data[["Status"]],
      levels = c(
        "error", "warning",
        "success", "skip"
      )
    )) |>
    dplyr::arrange(factor(.data[["Status"]]))

  #Render the summary.html
  if (summary) {
    render_summary(input = summary_df, summary_dir = summary_dir)
  }

  return(invisible(summary_df))
}

# Function to execute a single script
#' @noRd
execute_single_script <- function(script, verbose = TRUE, ...) {

  # Force cli to use ANSI colors
  old_ops <- options(cli.num_colors = 256)
  on.exit(options(old_ops))


  not_null <- function(x) {
    if (length(x) == 0 | is.null(x)) {
      return("")
    }

    x_max <- x[1:4]
    msg_ <- paste(x_max[!is.na(x_max)], collapse = "<br>")

    if (length(x) > 4) {
      msg_ <- paste(
        msg_,
        "<br>",
        "... See HTML logs for more details",
        collapse = ""
      )
    }
    return(msg_)
  }

  result <- tryCatch(
    {
      #cli::cli_alert_info(script)

      if (!whirl_file_exits()) {
        output <- run_script(script, ...)
      } else {
        output <- list(
          log_details = list(
            location = ""
          ),
          status = list(
            status = "skip",
            skip = "Skip because of error(s) in the previous step"
          )
        )
      }

      tibble::tibble(
        Directory = dirname(normalizePath(script, winslash = "/")),
        Filename = basename(normalizePath(script, winslash = "/")),
        Status = output$status$status,
        Hyperlink = if (output$log_details$location == "") {
          ""
        } else {
          normalizePath(output$log_details$location, winslash = "/")
        },
        Information = ""
      ) |>
        dplyr::mutate(Information = dplyr::case_when(
          output$status$status == "error" ~ not_null(output$status$error),
          output$status$status == "warning" ~ not_null(output$status$warning),
          output$status$status == "success" ~ not_null(output$status$success),
          output$status$status == "skip" ~ not_null(output$status$skip),
          TRUE ~ ""
        ))
    },
    error = function(e) {
      tibble::tibble(
        Directory = dirname(normalizePath(script, winslash = "/")),
        Filename = basename(normalizePath(script, winslash = "/")),
        Status = "error",
        Hyperlink = NA_character_,
        Information = conditionMessage(e)
      )
    }
  )

  if (verbose) {
  # Get the width of the screen
    width <- getOption("width")

    file_mes <- align_left(paste0(result$Directory, "/", result$Filename),
                           max(nchar(result$Filename) + 10, width - 20),
                           sep = ".")

    if (result$Status == "error") {cli::cli_alert_danger(paste0(cli::col_white(file_mes), cli::col_red("error")))
    } else if (result$Status == "warning") {cli::cli_alert_warning(paste0(cli::col_white(file_mes), cli::col_br_yellow("warning")))
    } else if (result$Status == "skip") {cli::cli_alert_warning(paste0(cli::col_white(file_mes), cli::col_br_blue("skip")))
    } else {cli::cli_alert_success(paste0(cli::col_white(file_mes), cli::col_green("done")))}
  }
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
  } else {
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
        hold[["Status"]] == "warning",
        "#fffaea",
        ifelse(hold[["Status"]] == "success", "#ebf5f1",
          ifelse(hold[["Status"]] == "skip", "#94CBFF", "white")
        )
      )
    )) |>
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = TRUE) |>
    knitr::knit_print()
}
