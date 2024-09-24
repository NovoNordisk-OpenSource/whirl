#' Execute single or multiple R, R Markdown, and Quarto scripts
#'
#' @description
#' Executes and logs the execution of the scripts. 
#' Logs for each script are stored in the same folder as the script.
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
#' @export

run <- function(input,
                steps = NULL,
                n_workers = options::opt("n_workers", env = "whirl"),
                summary_file = "summary.html"
                ) {

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
  queue <- whirl_queue$new(n_workers = n_workers)

  result <- internal_run(input = input,
                         steps = steps,
                         queue = queue,
                         level = 1)

  # Create the summary log
  summary_tibble <- util_queue_summary(result$queue)
  render_summary(input = summary_tibble, summary_file = summary_file)

  invisible(result$queue)
}













