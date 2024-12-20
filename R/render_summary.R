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

  summary_log_html <- withr::local_tempfile(fileext = ".html")

  summary_dir_f <- normalizePath(dirname(summary_file), winslash = "/")

  withr::with_dir(
    tempdir(),
    rmarkdown::render(
      input = summary_qmd,
      output_format = "html_document",
      output_file = summary_log_html,
      params = list(summary_df = input, summary_dir = summary_dir_f),
      quiet = TRUE
    )
  )

  # Create requested outputs
  tryCatch(
    file.copy(
      from = summary_log_html,
      to = summary_file,
      overwrite = TRUE
    ),
    error = function(e) {
      warning("File copy failed: ", e$message)
      FALSE
    }
  )
}

#' @noRd
knit_print.whirl_summary_info <- function(x, path_rel_start, ...) { # nolint
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
    formatted, "HTML Log"
  ))

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
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    knitr::knit_print()
}
