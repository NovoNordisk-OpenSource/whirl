#' RStudio IDE runner
#'
#' Easy to run and view log in RStudio. Also available as an addin. Takes
#' the active source document and [run()] it.
#'
#' @return Returns nothing. Run for side effects.
#' @seealso [run()]
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

  run_result <- run(input = list(list(names = basename(file),
                                      paths = file)),
                    verbosity_level = "minimal")

  log_file <- run_result[["result"]][[1]][["log_details"]][["location"]][[1]]
  if (file.exists(log_file)) {
    rstudioapi::viewer(log_file)
  }
}
