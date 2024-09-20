#' Render dataframe into a summary.html file
#'
#' @param input The input data.frame that should be rendered into a summary.html file
#' @param summary_file A character string specifying the path where the summary HTML file should be saved. Defaults to `"summary.html"`.
#'
#' @return Takes a dataframe as input and returns a log in html format
#'
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
  file_copy <- tryCatch(
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
