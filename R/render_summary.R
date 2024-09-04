#' Render dataframe into a summary.html file
#'
#' @param input The input data.frame that should be rendered into a summary.html file
#' @param summary_dir The output directory
#'
#' @return Takes a dataframe as input and returns a log in html format
#'
render_summary <- function(input, summary_dir) {

  if (tibble::is_tibble(input)) {
    input <- input |>
    dplyr::arrange(Directory, Filename)
  } else {
    input <- input
  }

  summary_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/summary.qmd", package = "whirl")),
    fileext = ".qmd")

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
      params = list(summary_df = input, summary_dir = summary_dir_f),
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

}
