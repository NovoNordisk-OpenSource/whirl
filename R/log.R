#' Run script
#' test with a single script
#'
#' @param script path
#' @inheritParams options_params
#' @param out_dir description
#'
#' @export

run_script <- function(script,
                       track_files = options::opt("track_files"),
                       check_renv = options::opt("check_renv"),
                       out_dir = dirname(script)) {

  # Input validation

  val <- checkmate::makeAssertCollection()

  checkmate::assert_character(x = script, any.missing = FALSE, len = 1, add = val)
  checkmate::assert_file_exists(x = script, access = "r", extension = c("R", "qmd", "Rmd"), add = val)

  checkmate::assert_logical(x = track_files, any.missing = FALSE, len = 1, add = val)
  checkmate::assert_logical(x = check_renv, any.missing = FALSE, len = 1, add = val)

  checkmate::assert_character(x = out_dir, any.missing = FALSE, len = 1, add = val)
  checkmate::assert_path_for_output(x = out_dir, overwrite = TRUE, add = val)

  track_files_discards <- options::opt("track_files_discards") |>
    checkmate::assert_character(any.missing = FALSE, add = val)

  checkmate::reportAssertions(val)

  # Derive execute directory for the quarto render process of the document
  # Abides to standards for R, Rmd, and qmd scripts,
  # in order for relative paths to work as expected inside the scripts.

  if (tools::file_ext(script) == "R") {
    quarto_execute_dir <- getwd()
  } else {
    quarto_execute_dir <- normalizePath(dirname(script))
  }

  # Derive output path and create out_dir if it does not exist

  if (!dir.exists(out_dir)) dir.create(out_dir)
  path_output <- file.path(out_dir, gsub(pattern = "\\.[^\\.]*$", replacement = ".html", x = basename(script)))

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
        strace_discards = track_files_discards,
        objects_path = objects_rds,
        renv = check_renv
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

  # Return object
  # Read in session info list

  objects_rds_lst <- readRDS(objects_rds) |>
    unlist(recursive = FALSE)

  output <- list(
    log_path = path_output,
    status = get_status(md = doc_md),
    session_info = objects_rds_lst
    )

  return(invisible(output))
}
