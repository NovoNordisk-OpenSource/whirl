#' RStudio IDE runner
#'
#' Easy to run and view log in RStudio. Also available as an addin. Takes
#' the active source document and whirl::run() it.
#'
#' @keywords intern
#' @noRd
run_current_script <- function() {
  stopifnot(
    "rstudioapi package needed" = requireNamespace("rstudioapi", quietly = TRUE)
  )

  # Run file
  script_info <- rstudioapi::getSourceEditorContext()
  file <- gsub(paste0(normalizePath(getwd()), .Platform$file.sep),
               "", normalizePath(script_info$path))

  run(input = file, verbosity_level = "minimal")

  if (grepl(".[Rr]$", file)) {
    log_file <- gsub(".R$", "_log.html", file, ignore.case = TRUE)
    if (file.exists(log_file))
      rstudioapi::viewer(log_file)
  }

  if (grepl(".Rmd$", file)) {
    log_file <- gsub(".Rmd$", "_log.html", file)
    if (file.exists(log_file))
      rstudioapi::viewer(log_file)
  }
}
