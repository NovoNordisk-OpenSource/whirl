#' Run script
#' test with a single script
#'
#' @param script path
#' @param strace logical
#' @param renv logical
#' @param out_dir description
#' @export

run_script <- function(script, track_files = FALSE, renv = TRUE, out_dir = dirname(script)) {

  stopifnot(file.exists(script))
  stopifnot(tools::file_ext(script) %in% c("R", "qmd", "Rmd"))
  stopifnot(is.logical(track_files))
  stopifnot(is.logical(renv))
  stopifnot(dir.exists(out_dir))

  # Create temp files for all documents.
  # Note: Documents are copied from package folder to make sure nothing is evaluated there.

  dummy_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/dummy.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  log_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/log.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  doc_md <- withr::local_tempfile(fileext = ".md")

  # TODO: Temp path to store strace

  log_html <- withr::local_tempfile(fileext = ".html")

  p <- callr::r_session$new()

  p$run(
    func = setwd,
    args = list(dir = tempdir())
  )

  p$run(getwd)

  # TODO: Start strace

  p$run(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = dummy_qmd,
      output_format = "markdown",
      output_file = basename(doc_md),
      execute_params = list(script = normalizePath(script)),
      execute_dir = getwd()
    )
  )

  # TODO: Stop strace?
  # TODO: Strace input path

  p$run(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = log_qmd,
      output_file = basename(log_html),
      execute_params = list(title = script, script_md = doc_md),
      execute_dir = getwd()
    )
  )

  p$close()

  output <- file.path(out_dir, gsub(pattern = "\\.[^\\.]*$", replacement = ".html", x = basename(script)))

  file.copy(
    from = log_html,
    to = output
  )

  return(invisible(output))
}

#' Internal log documents
#' @param doc name
#' @export

log_document <- function(doc) {
  system.file("documents", doc, package = "whirl")
}

#' Example scripts
#' @param doc name
#' @export

log_example <- function(doc) {
  system.file("examples", doc, package = "whirl")
}

#' Retrieve path of file from folder/subfolders
#'
#' @param in_file - file to search path for
#'
#' @return A path to a file
#' @export
#'
#' @examples
#' retrieve_fpath("prg1.R")
retrieve_fpath <- function(in_file) {
  tryCatch(
    expr = {
      list.files(
        pattern = in_file,
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE
      )[[1]] |> normalizePath(winslash = "/")
    },
    error = function(e){
      stop(paste("File does not seem to exist: ", in_file))
    }
  )
}
