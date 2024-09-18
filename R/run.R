#' Execute single or multiple R, R Markdown, and Quarto scripts
#'
#' @param input  A character vector of file path(s) to R, R Markdown, Quarto
#'   scripts, or files in a folder using regular expression, or to to a whirl
#'   config file. The input can also be structured in a list where each element
#'   will be executed in parallel.
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files is to be executed. Should be equivalent to the names
#'   of the steps found in the config file. If kept as NULL (default) then all
#'   steps listed in the config file will be executed.
#' @param n_workers Integer specifying the number of cores to use for parallel
#'   execution. If NULL (default), it will use the minimum of the total number
#'   of available cores - 1 and 8.
#' @param summary_dir A character string specifying the file path where the
#'   summary log will be stored. Default is the current working directory.
#' @param log_dir A character string of file path(s) specifying the directories
#'   where the logs from the individual script(s) will be stored. If only one
#'   folder is specified then all logs will be stored in the same folder. If
#'   multiple paths are specified this has to match the length of the input
#'   argument.
#' @return A tibble containing the execution results for all the scripts.
#'
#' @export
run <- function(input,
                steps = NULL,
                n_workers = NULL,
                log_dir = NULL,
                summary_dir = "."
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

  #Identify number of workers - Use one less than the total number of cores
  if (is.null(n_workers)) {
    n_workers <- min(c(parallel::detectCores() - 1, 8))
  }

  zephyr::msg("Executing scripts in parallel using {n_workers} cores\n",
              levels_to_write = "verbose",
              msg_fun = cli::cli_inform)

  # Initiating the queue
  queue <- whirl_queue$new(n_workers = n_workers)

  result <- internal_run(input = input,
                         steps = steps,
                         queue = queue,
                         level = 1)

  invisible(result$queue)
}

# TODO
# Place log in the script folder and allow user configuration
# Create the summary log













