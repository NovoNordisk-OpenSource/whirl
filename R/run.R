#' Execute single or multiple R, R Markdown, and Quarto scripts
#'
#' @description
#' Executes and logs the execution of the scripts.
#' Logs for each script are stored in the same folder as the script.
#'
#' The way the execution is logged is configurable through several options for
#' e.g. the verbosity of the logs. See [whirl-options] on how to configure these.
#'
#' @param input  A character vector of file path(s) to R, R Markdown, Quarto
#'   scripts, or files in a folder using regular expression, or to to a whirl
#'   config file. The input can also be structured in a list where each element
#'   will be executed sequentially, while scripts within each element can be
#'   executed in parallel.
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files (or list) is to be executed. Should be equivalent to
#'   the names of the steps found in the config file. If kept as NULL (default)
#'   then all steps listed in the config file will be executed.
#' @param summary_file A character string specifying the file path where the
#'   summary log will be stored.
#' @inheritParams whirl-options-params
#' @return A tibble containing the execution results for all the scripts.
#'
#' @examplesIf !is.null(quarto::quarto_path())
#'
#' # Start by copying the following three example scripts:
#' file.copy(
#'   from = system.file("examples", c("success.R", "warning.R", "error.R"),
#'     package = "whirl"
#'   ),
#'   to = "."
#' )
#'
#' # Run a single script
#' run("success.R")
#'
#' # Run several scripts in parallel on up to 2 workers
#' run(c("success.R", "warning.R", "error.R"), n_workers = 2)
#'
#' # Run scripts in two steps by providing them as list elements
#' run(
#'   list(
#'     c("success.R", "warning.R"),
#'     "error.R"
#'   ),
#'   n_workers = 2
#' )
#'
#' @examplesIf FALSE
#'
#' # Re-directing the logs to a sub-folder by utilizing the log_dir argument in
#' # run(). This will require that the sub-folder exist and the code is
#' # therefore not executed
#'
#' # Specifying the path using a manually defined character
#' run("success.R", log_dir = getwd())
#'
#' # Specifying the path with a generic function that can handle the scripts
#' # individually.
#' run("success.R", log_dir = function(x) {
#'   paste0(dirname(x), "/logs")
#' })
#'
#' @export

run <- function(
    input,
    steps = NULL,
    summary_file = "summary.html",
    n_workers = zephyr::get_option("n_workers", "whirl"),
    check_renv = zephyr::get_option("check_renv", "whirl"),
    verbosity_level = zephyr::get_verbosity_level("whirl"),
    track_files = zephyr::get_option("track_files", "whirl"),
    out_formats = zephyr::get_option("out_formats", "whirl"),
    log_dir = zephyr::get_option("log_dir", "whirl")) {
  # Additional Settings
  track_files_discards <- zephyr::get_option("track_files_discards") |>
    c(.libPaths()) # Don't track the library paths
  track_files_keep <- zephyr::get_option("track_files_keep")
  approved_pkgs_folder <- zephyr::get_option("approved_pkgs_folder")
  approved_pkgs_url <- zephyr::get_option("approved_pkgs_url")

  # Message when initiating
  d <- NULL
  zephyr::msg_verbose(
    message = "Executing scripts and generating logs",
    theme = list(
      rule = list(color = "skyblue3", "line-type" = "double")
    ),
    msg_fun = \(message, theme, .envir) {
      d <<- cli::cli_div(theme = theme, .auto_close = FALSE)
      cli::cli_rule(message, .envir = .envir)
    }
  )

  # Message when ending
  on.exit({
    zephyr::msg_verbose(
      message = "End of process",
      div = d,
      msg_fun = \(message, div, .envir) {
        cli::cli_rule(message, .envir = .envir)
        cli::cli_end(div)
      }
    )
  })

  # Constrain the number of workers
  n_workers <- min(128, n_workers)

  zephyr::msg_verbose(
    message = "Executing scripts in parallel using {n_workers} cores"
  )

  # Initiating the queue
  queue <- whirl_queue$new(
    n_workers = n_workers,
    check_renv = check_renv,
    verbosity_level = verbosity_level,
    track_files = track_files,
    out_formats = out_formats,
    track_files_discards = track_files_discards,
    track_files_keep = track_files_keep,
    approved_pkgs_folder = approved_pkgs_folder,
    approved_pkgs_url = approved_pkgs_url,
    log_dir = log_dir
  )

  result <- internal_run(
    input = input,
    steps = steps,
    queue = queue,
    level = 1
  )

  # Create the summary log if required
  if (!is.null(summary_file)) {
    summary_tibble <- util_queue_summary(result$queue)
    render_summary(input = summary_tibble, summary_file = summary_file)
  }

  invisible(result$queue)
}
