#' Run script
#' test with a single script
#'
#' @param script path
#' @inheritParams options_params
#' @param out_dir description
#'
#' @export

run_script <- function(script,
                       track_files = NULL,
                       check_renv = NULL,
                       out_formats = NULL,
                       approved_pkgs_folder = NULL,
                       approved_pkgs_url = NULL,
                       out_dir = dirname(script)) {
  # Use options as applicable
  if (is.null(track_files)) track_files <- options::opt("track_files")
  if (is.null(check_renv)) check_renv <- options::opt("check_renv")
  if (is.null(out_formats)) out_formats <- options::opt("out_formats")
  if (is.null(approved_pkgs_folder)) approved_pkgs_folder <- options::opt("approved_pkgs_folder")
  if (is.null(approved_pkgs_url)) approved_pkgs_url <- options::opt("approved_pkgs_url")
  track_files_discards <- options::opt("track_files_discards")
  track_files_keep <- options::opt("track_files_keep")

  # Input validation
  assert_run_script_input()

  # Run from whirl_r_session
  p <- whirl_r_session$new(
    verbose = TRUE,
    check_renv = check_renv,
    track_files = track_files,
    track_files_discards = track_files_discards,
    track_files_keep = track_files_keep,
    approved_pkgs_folder = approved_pkgs_folder,
    approved_pkgs_url = approved_pkgs_url)

  p$
    log_script(script = script)$
    wait()$
    check_status()

  p$
    create_log()$
    wait()$
    check_status()

  p$
    log_finish()$
    create_outputs(out_dir = out_dir, format = out_formats)

}

assert_run_script_input <- function(env = parent.frame()) {

  withr::with_environment(env, {
    script <- script
    track_files <- track_files
    check_renv <- check_renv
    out_formats <- out_formats
    out_dir <- out_dir
    approved_pkgs_folder <- approved_pkgs_folder
    approved_pkgs_url <- approved_pkgs_url
    track_files_discards <- track_files_discards
    track_files_keep <- track_files_keep
    }
  )

  val <- checkmate::makeAssertCollection()

  checkmate::assert_character(x = script, any.missing = FALSE, len = 1, add = val)
  checkmate::assert_file_exists(x = script, access = "r", extension = c("R", "qmd", "Rmd"), add = val)

  checkmate::assert_logical(x = track_files, any.missing = FALSE, len = 1, add = val)
  if (track_files) checkmate::assert_true(Sys.info()[["sysname"]] == "Linux", add = val)

  checkmate::assert_logical(x = check_renv, any.missing = FALSE, len = 1, add = val)

  checkmate::assert_character(x = out_formats, any.missing = FALSE, min.len = 1, add = val)
  checkmate::assert_subset(x = out_formats, choices = c("html", "json", "gfm", "markua", "commonmark"), empty.ok = FALSE, add = val)

  checkmate::assert_character(x = out_dir, any.missing = FALSE, len = 1, add = val)
  checkmate::assert_path_for_output(x = out_dir, overwrite = TRUE, add = val)

  checkmate::assert_character(x = approved_pkgs_folder, len = 1, null.ok = TRUE, add = val)
  checkmate::assert_character(x = approved_pkgs_url, len = 1, null.ok = TRUE, add = val)

  checkmate::assert_character(x = track_files_discards, any.missing = FALSE, add = val)

  checkmate::assert_character(x = track_files_keep, any.missing = FALSE, add = val)

  zephyr::report_checkmate_assertations(collection = val, env = env)
}
