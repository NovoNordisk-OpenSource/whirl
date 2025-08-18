#' Start a queue progress bar for a tag if verbose and possible
#' @noRd
pb_start <- function() {
  if (
    !cli::is_dynamic_tty() || zephyr::get_verbosity_level("whirl") != "verbose"
  ) {
    return(invisible())
  }

  withr::local_options(
    cli.progress_show_after = 0
  )

  cli::cli_progress_bar(
    type = "custom",
    format = "{cli::pb_spin} Running {cli::pb_extra$running} [{cli::pb_elapsed}]",
    extra = list(running = c()),
    .auto_close = FALSE
  )
}

#' Easily update the progress bar for the queue
#' @noRd
pb_update <- function(id, queue) {
  if (is.null(id)) {
    return(invisible())
  }

  cli::cli_progress_update(
    extra = list(
      running = basename(queue$script)[queue$status == "running"]
    ),
    id = id
  )
}

#' Convenience wrapper to end progress bar
#' @noRd
pb_done <- function(id) {
  if (is.null(id)) {
    return(invisible())
  }
  cli::cli_progress_done(id = id)
  invisible()
}
