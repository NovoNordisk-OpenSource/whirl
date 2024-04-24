#' Run script
#' test with a single script
#'
#' @param script path
#' @param track_files logical
#' @param renv logical
#' @param strace_discards keywords to use to discard not required lines
#' @param out_dir description
#'
#' @export

run_script <- function(script, track_files = FALSE, strace_discards = NULL, renv = FALSE, out_dir = dirname(script)) {

  # Input validation

  stopifnot(is.character(script) && file.exists(script) && tools::file_ext(script) %in% c("R", "qmd", "Rmd"))
  stopifnot(is.logical(track_files) && (!track_files | Sys.info()[["sysname"]] == "Linux"))
  stopifnot(is.logical(renv))
  stopifnot(is.character(out_dir) && dir.exists(out_dir))

  # Derive execute directory for the quarto render process of the document
  # Abides to standards for R, Rmd, and qmd scripts,
  # in order for relative paths to work as expected inside the scripts.

  if (tools::file_ext(script) == "R") {
    quarto_execute_dir <- getwd()
  } else {
    quarto_execute_dir <- normalizePath(dirname(script))
  }

  # Derive output path

  path_output <- file.path(out_dir, gsub(pattern = "\\.[^\\.]*$", replacement = ".html", x = basename(script)))
  objects_output <- file.path(out_dir, gsub(pattern = "\\.[^\\.]*$", replacement = ".rds", x = basename(script)))

  # Create temp files for all documents.
  # Note: Documents are copied from package folder to make sure nothing is evaluated there.
  # Follows recommendation from https://github.com/quarto-dev/quarto-r/issues/81#issuecomment-1375691267

  dummy_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/dummy.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  log_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/log.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  doc_md <- withr::local_tempfile(fileext = ".md")

  log_html <- withr::local_tempfile(fileext = ".html")

  objects_rds <- withr::local_tempfile(fileext = ".rds")

  # Create new R session used to run all documents

  p <- callr::r_session$new()

  # If track_files start strace tracking the process and which files are used

  if (track_files){

    strace_log <- withr::local_tempfile(fileext = ".strace")

    start_strace(pid = p$get_pid(), file = strace_log)

  } else {

    strace_log <- ''
  }

  # Run the input script and create markdown document with the output and session information.
  # withr::with_dir is used to temporarily change the working directory of the sub session
  # making sure content to be included in the log is saved in the temp dir.
  # Meanwhile execute_dir is used to execute script in the right directory.

  p$run(
    func = \(dir, ...) withr::with_dir(dir, quarto::quarto_render(...)),
    args = list(
      dir = tempdir(),
      input = basename(dummy_qmd),
      output_format = "markdown",
      output_file = basename(doc_md),
      execute_params = list(script = normalizePath(script)),
      execute_dir = quarto_execute_dir
    )
  )

  # Create the final log with extra information

  p$run(
    func = \(dir, ...) withr::with_dir(dir, quarto::quarto_render(...)),
    args = list(
      dir = tempdir(),
      input = log_qmd,
      output_file = basename(log_html),
      execute_params = list(
        title = script,
        script_md = doc_md,
        strace = track_files,
        strace_path = strace_log,
        strace_discards = strace_discards,
        objects_path = objects_rds,
        renv = renv
        ),
      execute_dir = getwd()
    )
  )

  # Close R session

  p$close()

  # Copy created log to output directory

  file.copy(
    from = log_html,
    to = path_output,
    overwrite = TRUE
  )

  # Read in session info list
  objects_rds_lst <- readRDS(objects_rds) |>
    unlist(recursive = FALSE)


  return(invisible(list(log_path = path_output, session_info = objects_rds_lst)))
}
