#' Render data.frame into a summary.html file
#'
#' @param input The input data.frame that should be rendered into a summary.html
#' file
#' @param summary_file A character string specifying the path where the summary
#' HTML file should be saved. Defaults to `"summary.html"`.
#'
#' @return Takes a data.frame as input and returns a log in html format
#' @noRd
render_summary <- function(input, summary_file = "summary.html") {
  summary_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/summary.qmd", package = "whirl")),
    fileext = ".qmd"
  )

  summary_log_html <- gsub(
    pattern = "qmd",
    replacement = "html",
    x = basename(summary_qmd)
  )

  summary_dir_f <- normalizePath(dirname(summary_file), winslash = "/")
  my_summaries <- knit_print_whirl_summary_info(input, summary_dir_f)
  withr::with_dir(
    tempdir(),
    {
      tryCatch(
        {
          quarto::quarto_render(
            input = summary_qmd,
            output_format = "html",
            execute_params = list(
              summary_df = my_summaries
            ),
            quiet = TRUE
          )
        },
        error = function(e) {
          cli::cli_abort(
            "Failed to render summary file {.file {summary_dir_f}}:
           {e$message}"
          )
        }
      )
    }
  )

  # Create requested outputs
  tryCatch(
    {
      file.copy(
        from = file.path(dirname(summary_qmd), summary_log_html),
        to = summary_file,
        overwrite = TRUE
      )
    },
    error = function(e) {
      warning("File copy failed: ", e$message)
      FALSE
    }
  )
}

#' @noRd
knit_print_whirl_summary_info <- function(x, path_rel_start, ...) {
  hold <- x |>
    data.frame(check.names = FALSE)

  row.names(hold) <- NULL
  ncols <- ncol(hold)

  if (grepl("rstudio_cloud", Sys.getenv("R_CONFIG_ACTIVE"))) {
    formatted <- file.path("/file_show?path=", hold$"Hyperlink")
  } else {
    formatted <- lapply(hold$"Hyperlink", path_rel, start = path_rel_start) |>
      unlist() |>
      file.path()
  }

  hold$Hyperlink <- paste0(sprintf(
    '<a href="%s" target="_blank">%s</a>',
    formatted,
    "HTML Log"
  ))

  knitr::kable(hold, format = "html", escape = FALSE) |>
    kableExtra::column_spec(
      1:ncols,
      background = ifelse(
        hold[["Status"]] == "error",
        "#fceeef",
        ifelse(
          hold[["Status"]] == "warning",
          "#fffaea",
          ifelse(
            hold[["Status"]] == "success",
            "#ebf5f1",
            ifelse(hold[["Status"]] == "skip", "#94CBFF", "white")
          )
        )
      )
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    )
}
