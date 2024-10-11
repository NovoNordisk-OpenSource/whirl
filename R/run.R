#' Execute single or multiple R, R Markdown, and Quarto scripts
#'
#' @description
#' Executes and logs the execution of the scripts.
#' Logs for each script are stored in the same folder as the script.
#'
#' The way the execution is logged is configurable through several options for
#' e.g. the verbosity of the logs. See [options] on how to configure these.
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
#' @inheritParams options_params
#' @return A tibble containing the execution results for all the scripts.
#'
#'@examplesIf FALSE
#'
#' # Run a single script
#' script <- system.file("examples/simple/success.R", package = "whirl")
#' run(script)
#'
#' # Run several scripts in parallel on up to 2 workers
#' scripts <- system.file("examples/simple", c("success.R", "warning.R", "error.R"), package = "whirl")
#' run(scripts, n_workers = 2)
#'
#' # Run scripts in several steps
#' step_1 <- system.file("examples/simple", c("success.R", "warning.R"), package = "whirl")
#' step_2 <- system.file("examples/simple", c("error.R"), package = "whirl")
#' run(list(step_1, step_2), n_workers = 2)
#'
#' @export

run <- function(input,
                steps = NULL,
                summary_file = "summary.html",
                n_workers = options::opt("n_workers", env = "whirl"),
                check_renv = options::opt("check_renv", env = "whirl"),
                verbosity_level = options::opt("verbosity_level", env = "whirl"),
                track_files = options::opt("track_files", env = "whirl"),
                out_formats = options::opt("out_formats", env = "whirl")
                ) {

  # Additional Settings
  track_files_discards = options::opt("track_files_discards", env = "whirl")
  track_files_keep = options::opt("track_files_keep", env = "whirl")
  approved_pkgs_folder = options::opt("approved_pkgs_folder", env = "whirl")
  approved_pkgs_url = options::opt("approved_pkgs_url", env = "whirl")

  # Options
  # opt_vec <- c("check_renv", "verbosity_level", "track_files", "track_files_discards",
  #              "track_files_keep", "approved_pkgs_folder", "approved_pkgs_url")
  #
  # initial_opt <- options::opts(opt_vec, env = "whirl")
  # names(initial_opt) <- paste0("whirl.", names(initial_opt))
  #
  # new_op <- options(whirl.check_renv = check_renv,
  #                   whirl.verbosity_level = verbosity_level,
  #                   whirl.track_files = track_files,
  #                   whirl.track_files_discards = track_files_discards,
  #                   whirl.track_files_keep = track_files_keep,
  #                   whirl.approved_pkgs_folder = approved_pkgs_folder,
  #                   whirl.approved_pkgs_url = approved_pkgs_url)

  # Message when initiating
  d <- cli::cli_div(theme = list(rule = list(
    color = "skyblue3", "line-type" = "double"
  )))

  zephyr::msg("Executing scripts and generating logs",
              levels_to_write = "verbose",
              msg_fun = cli::cli_rule)

  # Message when ending
  on.exit(zephyr::msg("End of process",
                      levels_to_write = "verbose",
                      msg_fun = cli::cli_rule))
  on.exit(cli::cli_end(d), add = TRUE)

  # Constrain the number of workers
  n_workers <- min(parallelly::availableCores(omit = 1), n_workers)

  zephyr::msg("Executing scripts in parallel using {n_workers} cores\n",
              levels_to_write = "verbose",
              msg_fun = cli::cli_inform)

  # Initiating the queue
  queue <- whirl_queue$new(n_workers = n_workers,
                           check_renv = check_renv,
                           verbosity_level = verbosity_level,
                           track_files = track_files,
                           out_formats = out_formats,
                           track_files_discards = track_files_discards,
                           track_files_keep = track_files_keep,
                           approved_pkgs_folder = approved_pkgs_folder,
                           approved_pkgs_url = approved_pkgs_url)

  result <- internal_run(input = input,
                         steps = steps,
                         queue = queue,
                         level = 1)

  # Create the summary log
  summary_tibble <- util_queue_summary(result$queue)
  render_summary(input = summary_tibble, summary_file = summary_file)

  invisible(result$queue)

  # Ensure that the options are reset on exit
  # on.exit(options(initial_opt))
}
