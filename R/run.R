#' Execute single or multiple R, R Markdown, and Quarto scripts by pointing
#' directly to the file(s) or by pointing to a whirl config file
#'
#' @param path  A character vector of file path(s) to R, R Markdown, Quarto
#'   scripts, or to folder(s), or to to a whirl config file.
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files is to be executed. Should be equivalent to the names
#'   of the steps found in the config file. If kept as NULL (default) then all
#'   stpes listed in the config file wil lbe executed.
#' @param parallel Logical; if TRUE, scripts will be executed in parallel.
#'   Default is FALSE. where the summary log will be stored.
#' @param num_cores Integer specifying the number of cores to use for parallel
#'   execution. If NULL (default), it will use one less than the total number of
#'   available cores.
#' @param summary_dir A character string specifying the file path where the
#'   summary log will be stored.
#' @param log_dir A character string of file path(s) specifying the directories
#'   where the logs from the individual script(s) will be stored. If only one
#'   folder is specified then all logs will be stored in the same folder. If
#'   multiple paths are specified this has to match the number of paths
#'   specified in the "paths" argument.
#' @return A list containing the execution results for each script. Each element
#'   of the list is a character string indicating the success or failure of the
#'   script execution.
#'
#' @export
run <- function(path,
                log_dir,
                steps = NULL,
                parallel = TRUE,
                num_cores = NULL,
                summary_dir = ".") {

  # Message when initiating
  cli::cli_h1("Executing scripts and generating logs")

  # Message when ending
  on.exit(cli::cli_h1("End of process"))

  # Ensure that any whirl_error file is removed before execution
  unlink_whirl_error_file()

  # Separating config and non-config files
  config_file <- path[grepl("yaml|yml|json", tools::file_ext(path))]
  non_config_files <- path[!grepl("yaml|yml|json", tools::file_ext(path))]

  # When only pointing to a whirl config file ----------------------------------
  if (length(config_file) > 0 & rlang::is_empty(non_config_files)) {
    run_by_config(file = config_file,
                  steps = steps,
                  summary_dir = summary_dir)
  }

  # When path do not point to any config file ----------------------------------
  if (rlang::is_empty(config_file) & length(non_config_files) > 0) {
    run_paths(paths = non_config_files,
              parallel = parallel,
              num_cores = num_cores,
              summary_dir = summary_dir)
  }


  # When pointing to a whirl config file plus additional files -----------------
  if (length(config_file) > 0 & length(non_config_files) > 0) {
    from_config <- run_by_config(file = config_file,
      steps = steps,
      summary_dir = summary_dir,
      summary = FALSE)

    cat("\n") #Ensure that the next message appear on a new line

    from_non_config <- run_paths(paths = non_config_files,
      parallel = parallel,
      num_cores = num_cores,
      summary_dir = summary_dir,
      summary = FALSE)

    got <- from_config |>
      dplyr::bind_rows(from_non_config)

    render_summary(got, summary_dir = summary_dir)


  }

  # Clean up when it ends
  unlink_whirl_error_file()

}



