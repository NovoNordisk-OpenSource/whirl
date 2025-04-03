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
      verbosity_level = zephyr::get_option("verbosity_level", "whirl"),
      check_renv = zephyr::get_option("check_renv", "whirl"),
      track_files = zephyr::get_option("track_files", "whirl"),
      out_formats = zephyr::get_option("out_formats", "whirl"),
      track_files_discards = zephyr::get_option(
        "track_files_discards",
        "whirl"
      ),
      track_files_keep = zephyr::get_option("track_files_keep", "whirl"),
      approved_pkgs_folder = zephyr::get_option(
        "approved_pkgs_folder",
        "whirl"
      ),
      approved_pkgs_url = zephyr::get_option("approved_pkgs_url", "whirl"),
      log_dir = zephyr::get_option("log_dir", "whirl"),
      wait_timeout = zephyr::get_option("wait_timeout", "whirl")
      # jscpd:ignore-end
    ) {
      wrs_initialize(
        verbosity_level,
        check_renv,
        track_files,
        out_formats,
        track_files_discards,
        track_files_keep,
        approved_pkgs_folder,
        approved_pkgs_url,
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

    #' @description Update the progress bar
    #' @param ... parsed to [cli::cli_progress_update()]
    #' @return [invisible] self
    pb_update = \(...) {
      wrs_pb_update(..., self = self, private = private, super = super)
    },

    #' @description Finalise the progress bar
    #' @param status Status of the script. success, warning, or error
    #' @return [invisible] self
    pb_done = \(status) {
      wrs_pb_done(status, self, private, super)
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

    #' @description Finish the log
    #' @return [invisible] self
    log_finish = \() {
      wrs_log_finish(self, private, super)
    },

    #' @description Create all log outputs
    #' @param out_dir [character] Output directory for the log
    #' @param format [character] Output formats to create
    #' @return [invisible],[list] of logging information
    create_outputs = \(
      out_dir,
      format = zephyr::get_option("out_formats", "whirl")
    ) {
      wrs_create_outputs(out_dir, format, self, private, super)
    }
  ),
  private = list(
    #' @description Finalize the whirl R session
    finalize = \() {
      wrs_finalize(self, private, super)
    },
    verbosity_level = NULL,
    wd = NULL,
    track_files = NULL,
    out_formats = NULL,
    log_dir = NULL,
    track_files_discards = NULL,
    track_files_keep = NULL,
    approved_pkgs_folder = NULL,
    approved_pkgs_url = NULL,
    check_renv = NULL,
    pb = NULL,
    current_script = NULL
  ),
  inherit = callr::r_session
)

wrs_initialize <- function(
  verbosity_level,
  check_renv,
  track_files,
  out_formats,
  track_files_discards,
  track_files_keep,
  approved_pkgs_folder,
  approved_pkgs_url,
  log_dir,
  wait_timeout,
  self,
  private,
  super
) {
  # uses callr::r_session$initialize()
  super$initialize(wait_timeout = wait_timeout)

  private$wd <- withr::local_tempdir(clean = FALSE)
  private$verbosity_level <- verbosity_level
  private$check_renv <- check_renv
  private$track_files <- track_files
  private$out_formats <- out_formats
  private$track_files_discards <- track_files_discards
  private$track_files_keep <- track_files_keep
  private$approved_pkgs_folder <- approved_pkgs_folder
  private$approved_pkgs_url <- approved_pkgs_url
  private$log_dir <- log_dir

  # If the stream does not support dynamic tty, which is needed for progress
  # bars to update in place, the verbosity is downgraded.
  if (private$verbosity_level == "verbose" && !cli::is_dynamic_tty()) {
    private$verbosity_level <- "minimal"
  }

  super$run(func = setwd, args = list(dir = private$wd))

  system.file("documents", package = "whirl") |>
    list.files(full.names = TRUE) |>
    file.copy(to = private$wd)

  super$run(
    func = Sys.setenv,
    args = list(WHIRL_LOG_MSG = file.path(private$wd, "log_msg.json"))
  )

  environment_file <- file.path(private$wd, "_environment")
  # Add whirl log file to environment file
  cat(
    sprintf(
      "WHIRL_LOG_MSG='%s'",
      file.path(private$wd, "log_msg.json")
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
    "Working Directory: {private$wd}",
    "Verbose: {private$verbosity_level}"
  )

  cli::cli_bullets(
    c(
      "<whirl_r_session> object",
      rlang::set_names(msg, "*")
    )
  )

  return(invisible(self))
}

wrs_pb_update <- function(..., self, private, super) {
  if (!is.null(private$pb)) private$pb$update(...)
  return(invisible(self))
}

wrs_pb_done <- function(status, self, private, super) {
  if (!is.null(private$pb)) private$pb$done(status)
  return(invisible(self))
}

wrs_poll <- function(timeout, self, private, super) {
  status <- super$poll_process(timeout)
  if (status == "timeout") self$pb_update()
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
    status$error |>
      as.character() |>
      rlang::abort()
  }
  return(invisible(status))
}

wrs_log_script <- function(script, self, private, super) {
  private$current_script <- script

  # Set the execute directory of the Quarto process calling the script
  quarto_execute_dir <- zephyr::get_option("execute_dir", "whirl")
  if (is.null(quarto_execute_dir)) {
    quarto_execute_dir <- switch(
      get_file_ext(script),
      "R" = getwd(),
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

  # Execute the script

  if (private$verbosity_level != "quiet") {
    private$pb <- pb_script$new(
      script = private$current_script,
      use_progress = private$verbosity_level == "verbose"
    )
  }

  self$pb_update(status = "Running script")

  self$call(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = "dummy.qmd",
      output_format = "markdown",
      output_file = "doc.md",
      execute_params = list(
        script = normalizePath(script),
        with_library_paths = .libPaths(),
        check_approved_folder_pkgs = private$check_approved_folder_pkgs,
        check_approved_url_pkgs = private$check_approved_url_pkgs,
        renv = private$check_renv,
        tmpdir = private$wd
      ),
      execute_dir = quarto_execute_dir
    )
  )

  return(invisible(self))
}

wrs_create_log <- function(self, private, super) {
  self$pb_update(status = "Creating log")

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
        check_approved_folder_pkgs = private$approved_pkgs_folder,
        check_approved_url_pkgs = private$approved_pkgs_url,
        with_library_paths = .libPaths(),
        tmpdir = private$wd
      ),
      execute_dir = getwd()
    )
  )

  return(invisible(self))
}

wrs_log_finish <- function(self, private, super) {
  if (!is.null(private$pb)) {
    status <- self$get_wd() |>
      file.path("doc.md") |>
      get_status()

    self$pb_done(status = status[["status"]])
  }

  return(invisible(self))
}

wrs_create_outputs <- function(out_dir, format, self, private, super) {
  # Create R object for return

  output <- list(
    status = file.path(self$get_wd(), "doc.md") |>
      get_status(),
    session_info_rlist = file.path(self$get_wd(), "objects.rds") |>
      readRDS() |>
      unlist(recursive = FALSE),
    log_details = list(
      location = file.path(
        out_dir,
        gsub(
          pattern = "\\.[^\\.]*$",
          replacement = "_log.html",
          x = basename(private$current_script)
        )
      ),
      script = private$current_script
    )
  )

  # Create requested outputs

  if ("html" %in% format) {
    file.copy(
      from = file.path(self$get_wd(), "log.html"),
      to = file.path(
        out_dir,
        gsub(
          pattern = "\\.[^\\.]*$",
          replacement = "_log.html",
          x = basename(private$current_script)
        )
      ),
      overwrite = TRUE
    )
  }

  if (any(c("gfm", "commonmark", "markua") %in% format)) {
    mdformats(
      script = private$current_script,
      log_html = file.path(self$get_wd(), "log.html"),
      mdfmt = format[format %in% c("gfm", "commonmark", "markua")],
      out_dir = out_dir,
      self = self
    )
  }

  if ("json" %in% format) {
    jsonlite::write_json(
      x = output,
      force = TRUE,
      pretty = TRUE,
      path = file.path(
        out_dir,
        gsub(
          pattern = "\\.[^\\.]*$",
          replacement = "_log.json",
          x = basename(private$current_script)
        )
      )
    )
  }

  # Return logs from strace or whirl
  file.copy(
    from = file.path(self$get_wd(), "log_msg.json"),
    to = file.path(
      out_dir,
      gsub(
        pattern = "\\.[^\\.]*$",
        replacement = "_msg_log.json",
        x = basename(private$current_script)
      )
    )
  )

  return(invisible(output))
}
