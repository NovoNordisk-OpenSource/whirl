# Progress bar (spinner) when running a single script
#' @noRd
pb_script <- R6::R6Class(
  classname = "pb_script",
  public = list(
    initialize = \(script, use_progress = cli::is_dynamic_tty()) {
      withr::local_options(
        cli.progress_show_after = 0,
        cli.progress_clear = FALSE
      )

      private$script <- script

      if (use_progress) {
        private$id <- cli::cli_progress_bar(
          type = "custom",
          clear = FALSE,
          status = "Started",
          current = FALSE,
          .auto_close = FALSE,
          extra = list(
            script = script,
            done = "ERROR"
          ),
          format = paste0(
            "{cli::pb_spin} ",
            "{.href [{basename(cli::pb_extra$script)}](file://{cli::pb_extra$script})}: ",  # nolint: line_length_linter
            "{cli::pb_status}",
            "[{cli::pb_elapsed}]"
          ),
          format_done = paste0(
            "{cli::pb_extra$done} ",
            "{.href [{basename(cli::pb_extra$script)}](file://{cli::pb_extra$script})}: ",  # nolint: line_length_linter
            "{cli::pb_status}",
            "[{cli::pb_elapsed}]"
          )
        )
        self$update()
      }
    },
    update = \(...) {
      if (!is.null(private$id)) cli::cli_progress_update(id = private$id, ...)
    },
    done = \(status = c("success", "warning", "error")) {
      status <- rlang::arg_match(status)
      done <- switch(status,
        success = cli::col_green(cli::symbol$tick),
        warning = cli::col_yellow(cli::symbol$warning),
        error = cli::col_red(cli::symbol$cross)
      )
      done_msg <- switch(status,
        success = "Completed succesfully",
        warning = "Completed with warnings",
        error = "Completed with errors"
      )

      if (!is.null(private$id)) {
        self$update(extra = list(done = done), status = done_msg)
        cli::cli_progress_done(id = private$id)
      } else {
        cli::cli_inform("{done} {basename(private$script)}: {done_msg}")
      }
    }
  ),
  private = list(
    id = NULL,
    script = NULL
  )
)
