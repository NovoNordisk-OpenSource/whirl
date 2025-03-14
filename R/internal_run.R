#' Sending the scripts to the whirl_r_queue for execution
#'
#' @param input A character vector of file path(s) to R, R Markdown, Quarto
#'   scripts, or files in a folder using regular expression, or to to a whirl
#'   config file. The input can also be structured in a list where each element
#'   will be executed in parallel.
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files is to be executed. Should be equivalent to the names
#'   of the steps found in the config file. If kept as NULL (default) then all
#'   steps listed in the config file will be executed.
#' @param queue The whirl_r_queue that should execute the scripts
#' @param level Depth of the recursive config calls.
#' The initial call will have 1.
#' @inheritParams options_params
#' @return A tibble containing the execution results for all the scripts.
#' @noRd
internal_run <- function(input,
                         steps,
                         queue,
                         level) {
  # Enrich the input with "name" and "path" elements
  enriched <- enrich_input(input, steps)

  # Loop over the elements
  for (i in seq_along(enriched)) {
    files <- enriched[[i]]$path
    name <- enriched[[i]]$name

    # Messages
    cli_level <- get(paste0("cli_h", min(level, 3)), envir = asNamespace("cli"))
    zephyr::msg_verbose(message = name, msg_fun = cli_level)

    # If the step points to a config file then re-initiate internal_run()
    if (any(grepl("yaml|yml", get_file_ext(files)))) {
      internal_run(
        input = files,
        steps = steps,
        queue = queue,
        level = level + 1
      )
    } else {
      # Execute the scripts
      queue$run(files)
      zephyr::msg_verbose(message = "\n", msg_fun = cli::cli_verbatim)
    }
  }

  invisible(queue)
}
