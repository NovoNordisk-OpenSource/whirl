#' Whirl R session
#' @description
#' Extension of [callr::r_session] with additional methods for easier creating logs.
#' @importFrom callr r_session
#' @importFrom R6 R6Class
#' @keywords internal

whirl_r_session <- R6::R6Class(
  classname = "whirl_r_session",
  public = list(

    #' @description Initialize the new whirl R session
    #' @param verbose [logical] Should the progress be printed to the console?
    #' @param check_renv [logical] Should renv be checked?
    #' @param track_files [logical] Should the files be tracked?
    #' @param track_files_discards [character] Files to discard from tracking
    #' @param track_files_keep [character] Files to keep from tracking
    #' @param approved_pkgs_folder [character] Folder with approved packages
    #' @param approved_pkgs_url [character] URL with approved packages
    #' @return A [whirl_r_session] object
    initialize = \(verbose = TRUE, check_renv = FALSE, track_files = FALSE, track_files_discards = c(), track_files_keep = c(), approved_pkgs_folder = c(), approved_pkgs_url = c()) {
      wrs_initialize(verbose, check_renv, track_files, track_files_discards, track_files_keep, approved_pkgs_folder, approved_pkgs_url, self, private, super)
    },

    #' @description Finalize the whirl R session
    finalize = \() {
      wrs_finalize(self, private, super)
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
    create_outputs = \(out_dir, format) {
      wrs_create_outputs(out_dir, format, self, private, super)
    }
  ),
  private = list(
    verbose = NULL,
    wd = NULL,
    track_files = NULL,
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

wrs_initialize <- function(verbose, check_renv, track_files, track_files_discards, track_files_keep, approved_pkgs_folder, approved_pkgs_url,
                           self, private, super) {
  super$initialize() # uses callr::r_session$initialize()

  # TODO: Is there a way to use `.local_envir` to avoid having to clean up the temp dir in finalize?
  private$wd <- withr::local_tempdir(clean = FALSE)
  private$verbose <- verbose
  private$check_renv <- check_renv
  private$track_files <- track_files
  private$track_files_discards <- track_files_discards
  private$track_files_keep <- track_files_keep
  private$approved_pkgs_folder <- approved_pkgs_folder
  private$approved_pkgs_url <- approved_pkgs_url

  super$run(func = setwd, args = list(dir = private$wd))
  super$run(func = options, args = list(whirl.log_msgs = data.frame(time = Sys.time()[0], op = character(), msg = character())))

  system.file("documents", package = "whirl") |>
    list.files(full.names = TRUE) |>
    file.copy(to = private$wd)

  environment_file <- file.path(private$wd, "_environment")
  environment_file |>
    readLines() |>
    glue::glue() |>
    writeLines(environment_file)

  if (track_files) {
    start_strace(pid = super$get_pid(), file = file.path(private$wd, "strace.log"))
  }
}

wrs_finalize <- function(self, private, super) {
  super$run(func = setwd, args = list(dir = getwd())) # Needed for Windows to be able to delete the temp wd
  unlink(private$wd, recursive = TRUE)
  super$finalize()
}

wrs_print <- function(self, private, super) {
  msg <- c(
    utils::capture.output(super$print()),
    "Working Directory: {private$wd}",
    "Verbose: {private$verbose}"
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
  if (!is.null(private$verbose)) private$pb$update(...)
  return(invisible(self))
}

wrs_pb_done <- function(status, self, private, super) {
  if (!is.null(private$verbose)) private$pb$done(status)
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
    if (timeout >= 0 && difftime(Sys.time(), start, units = "secs") > timeout) break
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

  if (private$verbose) {
    private$pb <- pb_script$new(script = private$current_script)
  }

  self$pb_update(status = "Running script")

  quarto_execute_dir <- switch(tools::file_ext(script),
    "R" = getwd(),
    normalizePath(dirname(script)) # TODO: Should this default be changed?
  )

  self$call(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = "dummy.qmd",
      output_format = "markdown",
      output_file = "doc.md",
      execute_params = list(
        script = normalizePath(script),
        with_library_paths = .libPaths()
      ),
      execute_dir = quarto_execute_dir
    )
  )

  return(invisible(self))
}

wrs_create_log <- function(self, private, super) {
  self$pb_update(status = "Creating log")

  self$call(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = "log.qmd",
      output_file = "log.html",
      execute_params = list(
        title = private$current_script,
        script_md = file.path(self$get_wd(), "doc.md"),
        p_wd = self$get_wd(),
        strace = private$track_files,
        strace_path = file.path(self$get_wd(), "strace.log"),
        strace_discards = private$track_files_discards,
        strace_keep = private$track_files_keep,
        objects_path = file.path(self$get_wd(), "objects.rds"),
        check_approved_folder_pkgs = private$approved_pkgs_folder,
        check_approved_url_pkgs = private$approved_pkgs_url,
        renv = private$check_renv,
        with_library_paths = .libPaths()
      ),
      execute_dir = getwd()
    )
  )

  return(invisible(self))
}

wrs_log_finish <- function(self, private, super) {
  if (private$verbose) {
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
    log_details = list(location = file.path(
      out_dir,
      gsub(
        pattern = "\\.[^\\.]*$",
        replacement = "_log.html",
        x = basename(private$current_script)
      )
    ))
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
      out_dir = out_dir
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
          replacement = ".json",
          x = basename(private$current_script)
        )
      )
    )
  }

  return(invisible(output))
}
