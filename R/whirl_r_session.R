#' Whirl R session
#' @description
#' Extension of [callr::r_session] with additional methods for easier creating
#' logs.
#' @importFrom R6 R6Class
#' @importFrom callr r_session
#' @noRd

whirl_r_session <- R6::R6Class(
  classname = "whirl_r_session",
  public = list(
    #' @description Initialize the new whirl R session
    #' @inheritParams options_params
    #' @return A [whirl_r_session] object
    initialize = \(
      # jscpd:ignore-start
      check_renv = zephyr::get_option("check_renv", "whirl"),
      track_files = zephyr::get_option("track_files", "whirl"),
      out_formats = zephyr::get_option("out_formats", "whirl"),
      track_files_discards = zephyr::get_option(
        "track_files_discards",
        "whirl"
      ),
      track_files_keep = zephyr::get_option("track_files_keep", "whirl"),
      approved_packages = zephyr::get_option("approved_packages", "whirl"),
      log_dir = zephyr::get_option("log_dir", "whirl"),
      wait_timeout = zephyr::get_option("wait_timeout", "whirl")
      # jscpd:ignore-end
    ) {
      wrs_initialize(
        check_renv,
        track_files,
        out_formats,
        track_files_discards,
        track_files_keep,
        approved_packages,
        log_dir,
        wait_timeout,
        self,
        private,
        super
      )
    },

    #' @description Print the whirl R session
    #' @return [invisible] self
    print = \() {
      wrs_print(self, private, super)
    },

    #' @description Poll the process
    #' @param timeout The timeout in milliseconds
    #' @return [invisible] self
    poll = \(timeout) {
      wrs_poll(timeout, self, private, super)
    },

    #' @description Wait for the process to complete
    #' @param timeout The timeout in milliseconds
    #' @return [invisible] self
    wait = \(timeout = -1) {
      wrs_wait(timeout, self, private, super)
    },

    #' @description Check the status of the callr process
    #' @return [invisible] status
    check_status = \() {
      wrs_check_status(self, private, super)
    },

    #' @description Log the script
    #' @param script The script to log
    #' @return [invisible] self
    log_script = \(script) {
      wrs_log_script(script, self, private, super)
    },

    #' @description Create the html log
    #' @return [invisible] self
    create_log = \() {
      wrs_create_log(self, private, super)
    },

    #' @description Finish the log and create outputs
    #' @param out_dir [character] Output directory for the log
    #' @param format [character] Output formats to create
    #' @return [invisible] self
    log_finish = \(
      out_dir,
      format = zephyr::get_option("out_formats", "whirl")
    ) {
      wrs_log_finish(out_dir, format, self, private, super)
    }
  ),
  private = list(
    #' @description Finalize the whirl R session
    finalize = \() {
      wrs_finalize(self, private, super)
    },
    wd = NULL,
    track_files = NULL,
    out_formats = NULL,
    log_dir = NULL,
    track_files_discards = NULL,
    track_files_keep = NULL,
    approved_packages = NULL,
    check_renv = NULL,
    current_script = NULL,
    track_files_log = "log_msg.json",
    result = NULL
  ),
  inherit = callr::r_session
)

wrs_initialize <- function(
  check_renv,
  track_files,
  out_formats,
  track_files_discards,
  track_files_keep,
  approved_packages,
  log_dir,
  wait_timeout,
  self,
  private,
  super
) {
  super$initialize(wait_timeout = wait_timeout) # uses callr::r_session$initialize()

  private$wd <- withr::local_tempdir(clean = FALSE)
  private$check_renv <- check_renv
  private$track_files <- track_files
  private$out_formats <- out_formats
  private$track_files_discards <- track_files_discards
  private$track_files_keep <- track_files_keep
  private$approved_packages <- approved_packages
  private$log_dir <- log_dir

  super$run(func = setwd, args = list(dir = private$wd))

  system.file("documents", package = "whirl") |>
    list.files(full.names = TRUE) |>
    file.copy(to = private$wd)

  super$run(
    func = Sys.setenv,
    args = list(WHIRL_LOG_MSG = file.path(private$wd, private$track_files_log))
  )

  saveRDS(
    object = options(),
    file = file.path(private$wd, "parent_options.rds")
  )

  environment_file <- file.path(private$wd, "_environment")
  # Add whirl log file to environment file
  cat(
    sprintf(
      "WHIRL_LOG_MSG='%s'",
      file.path(private$wd, private$track_files_log)
    ),
    file = environment_file,
    append = TRUE
  )

  if (track_files) {
    start_strace(
      pid = super$get_pid(),
      file = file.path(private$wd, "strace.log")
    )
  }

  zephyr::msg_debug(
    "Started session with pid={.field {self$get_pid()}} and wd={.file {private$wd}}" # nolint: line_length_linter
  )
}

wrs_finalize <- function(self, private, super) {
  zephyr::msg_debug(
    "Finalizing session with pid={.field {self$get_pid()}} and wd={.file {private$wd}}" # nolint: line_length_linter
  )
  super$run(func = setwd, args = list(dir = getwd()))
  unlink(private$wd, recursive = TRUE)
  super$finalize()
}

wrs_print <- function(self, private, super) {
  msg <- c(
    utils::capture.output(super$print()),
    "Working Directory: {private$wd}"
  )

  cli::cli_bullets(
    c(
      "<whirl_r_session> object",
      rlang::set_names(msg, "*")
    )
  )

  return(invisible(self))
}

wrs_poll <- function(timeout, self, private, super) {
  status <- super$poll_process(timeout)
  return(status)
}

wrs_wait <- function(timeout, self, private, super) {
  start <- Sys.time()
  timeout <- timeout / 1000 # Convert to secs
  go <- TRUE
  while (go) {
    go <- self$poll(timeout = 50) == "timeout"
    if (timeout >= 0 && difftime(Sys.time(), start, units = "secs") > timeout) {
      break
    }
  }
  return(invisible(self))
}

wrs_check_status <- function(self, private, super) {
  status <- super$read()
  if (!is.null(status$error)) {
    cli::cli_abort(
      c(
        "Could not run {.file {private$current_script}}",
        "i" = "Error from {.fn quarto::quarto_render}:",
        strsplit(x = status$stdout, split = "\n") |>
          unlist()
      )
    )
  }
  return(invisible(status))
}

wrs_log_script <- function(script, self, private, super) {
  zephyr::msg_debug("Running script {.file {script}}")
  private$current_script <- script

  saveRDS(
    # Log starting time
    object = Sys.time(),
    file = file.path(private$wd, "start.rds")
  )

  saveRDS(
    # Log script metadata
    object = list(
      name = private$current_script,
      md5sum = private$current_script |> # Devskim: ignore DS126858
        tools::md5sum() |> # Devskim: ignore DS126858
        unname(),
      content = readLines(private$current_script) |>
        paste0(collapse = "\n")
    ),
    file = file.path(private$wd, "script.rds")
  )

  # Set the execute directory of the Quarto process calling the script
  quarto_execute_dir <- zephyr::get_option("execute_dir", "whirl")
  if (is.null(quarto_execute_dir)) {
    quarto_execute_dir <- switch(
      get_file_ext(script),
      "R" = normalizePath("."),
      normalizePath(dirname(script))
    )
  } else if (is.function(quarto_execute_dir)) {
    quarto_execute_dir <- quarto_execute_dir(script)
  }

  if (!file.exists(quarto_execute_dir)) {
    cli::cli_abort(
      "Script {.val {script}} cannot be run because execute directory {.val {quarto_execute_dir}} does not exist" # nolint: line_length_linter
    )
  }

  self$call(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = "dummy.qmd",
      output_format = "markdown",
      output_file = "doc.md",
      execute_params = list(
        script = normalizePath(script),
        with_library_paths = .libPaths(),
        renv = private$check_renv,
        tmpdir = normalizePath(private$wd)
      ),
      execute_dir = quarto_execute_dir
    )
  )

  return(invisible(self))
}

wrs_create_log <- function(self, private, super) {
  zephyr::msg_debug("Creating log for {.file {private$current_script}}")

  if (private$track_files) {
    strace_msg <- private$wd |>
      file.path("strace.log") |>
      read_strace(p_wd = private$wd) |>
      refine_strace(
        strace_keep = private$track_files_keep,
        strace_discards = private$track_files_discards
      )

    con <- file(
      description = self$run(Sys.getenv, list("WHIRL_LOG_MSG")),
      open = "a"
    )
    jsonlite::stream_out(x = strace_msg, con = con, verbose = FALSE)
    close(con)
  }

  self$call(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = "log.qmd",
      output_file = "log.html",
      execute_params = list(
        title = private$current_script,
        approved_packages = private$approved_packages,
        with_library_paths = .libPaths(),
        track_files = private$track_files,
        tmpdir = normalizePath(private$wd)
      ),
      execute_dir = normalizePath(".")
    )
  )

  return(invisible(self))
}

wrs_log_finish <- function(out_dir, format, self, private, super) {
  private$result <- private$wd |>
    file.path("result.rds") |>
    readRDS()

  output <- wrs_create_outputs(out_dir, format, self, private, super)

  wrs_report_status(
    status = output$status$message,
    script = private$current_script,
    logs = output$logs
  )

  invisible(output)
}

wrs_report_status <- function(status, script, logs) {
  script_msg <- create_cli_links(
    text = basename(script),
    href = script
  )

  logs_msg <- create_cli_links(
    text = get_file_ext(logs),
    href = logs
  )

  switch(
    EXPR = status,
    success = zephyr::msg_success(
      "{script_msg}: Completed succesfully. See {cli::qty(logs_msg)}log{?s} {logs_msg}."
    ),
    warning = zephyr::msg(
      "{script_msg}: Completed with warnings. See {cli::qty(logs_msg)}log{?s} {logs_msg}.",
      msg_fun = cli::cli_alert_warning # Since zephyr::msg_warning only shows when verbose
    ),
    error = zephyr::msg_danger(
      "{script_msg}: Completed with errors. See {cli::qty(logs_msg)}log{?s} {logs_msg}."
    ),
    cli::cli_abort(
      "{script}: Completed with unknown status {.emph {status}}. See {cli::qty(logs_msg)}log{?s} {logs_msg}."
    )
  )
}

wrs_create_outputs <- function(out_dir, format, self, private, super) {
  output <- private$result
  output$logs <- wrs_create_logs(out_dir, format, output, self, private, super)
  return(invisible(output))
}

wrs_create_logs <- function(out_dir, format, output, self, private, super) {
  logs <- c()

  if ("html" %in% format) {
    html_log <- file.path(
      out_dir,
      gsub(
        pattern = "\\.[^\\.]*$",
        replacement = "_log.html",
        x = basename(private$current_script)
      )
    )
    file.copy(
      from = file.path(private$wd, "log.html"),
      to = html_log,
      overwrite = TRUE
    )
    logs <- c(logs, html_log)
  }

  if (any(c("gfm", "commonmark", "markua") %in% format)) {
    logs_md <- mdformats(
      script = private$current_script,
      log_html = file.path(private$wd, "log.html"),
      mdfmt = format[format %in% c("gfm", "commonmark", "markua")],
      out_dir = out_dir,
      self = self
    )
    logs <- c(logs, logs_md)
  }

  if ("json" %in% format) {
    json_log <- file.path(
      out_dir,
      gsub(
        pattern = "\\.[^\\.]*$",
        replacement = "_log.json",
        x = basename(private$current_script)
      )
    )

    jsonlite::write_json(
      x = output,
      force = TRUE,
      pretty = TRUE,
      path = json_log
    )

    logs <- c(logs, json_log)
  }

  return(invisible(logs))
}
