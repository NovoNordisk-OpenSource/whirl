#' Queue for continuous execution and logging of scripts
#' @description
#' Implementation of a queue for supporting the continuous execution and logging
#' of several scripts.
#' The queue can be used interactively, but is mainly designed to be the
#' internal backbone of the `run()` function.
#' When a queue has several workers, pushed scripts will be run in parallel.
#' @importFrom R6 R6Class
#' @noRd

whirl_queue <- R6::R6Class(
  classname = "whirl_queue",
  public = list(
    #' @inheritParams options_params
    #' @description Initialize the new whirl_queue
    #' @return A [whirl_queue] object
    initialize = \(
      # jscpd:ignore-start
      n_workers = zephyr::get_option("n_workers", "whirl"),
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
      log_dir = zephyr::get_option("log_dir", "whirl")
      # jscpd:ignore-end
    ) {
      wq_initialise(
        self,
        private,
        n_workers,
        verbosity_level,
        check_renv,
        track_files,
        out_formats,
        track_files_discards,
        track_files_keep,
        approved_pkgs_folder,
        approved_pkgs_url,
        log_dir
      )
    },

    #' @description Push scripts to the queue
    #' @param scripts [character] Full paths for the scripts to be executed
    #' @param tag (optional) [character] Tag for the scripts to include in
    #' the queue
    #' @return [invisible] self
    push = \(scripts, tag = NA_character_) {
      wq_push(self, private, scripts, tag)
    },

    #' @description Push scripts in the queue without executing them.
    #' Utility to include skipped scripts in the final queue.
    #' @param scripts [character] Full paths for the scripts to be executed
    #' @param tag (optional) [character] Tag for the scripts to include in
    #' the queue
    #' @return [invisible] self
    skip = \(scripts, tag = NA_character_) {
      wq_skip(self, private, scripts, tag)
    },

    #' @description Poll the queue and start next steps if needed
    #' @param timeout [numeric] The timeout in milliseconds.
    #' Note it is only implemented approximately if more than one script is
    #' running simultaneously.
    #' @return [character] Status of all scripts queue
    poll = \(timeout) {
      wq_poll(self, private, timeout)
    },

    #' @description Wait for the queue to complete
    #' @param timeout [numeric] The timeout in milliseconds
    #' @return [invisible] self
    wait = \(timeout = -1) {
      wq_wait(self, private, timeout)
    },

    #' @description Run scripts using the queue.
    #' This is a wrapper around calling both push() and wait().
    #' @param scripts [character] with full paths for the scripts to be executed
    #' @return [invisible] self
    run = \(scripts) {
      wq_run(scripts, self)
    },

    #' @description Print method displaying the current status of the queue
    #' @return [invisible] self
    print = \() {
      print(self$queue)
      return(invisible(self))
    }
  ),
  active = list(
    #' @field queue [tibble] Current status of the queue
    queue = \() {
      private$.queue
    },

    #' @field workers [tibble] Current status of the workers
    workers = \() {
      private$.workers
    },

    #' @field available_workers [integer] Which workers are available
    available_workers = \() {
      which(!self$workers$active)
    },

    #' @field next_ids [integer] Which scripts are next in the queue
    next_ids = \() {
      self$queue$id[self$queue$status == "waiting"] |>
        head(length(self$available_workers))
    },

    #' @field next_workers [integer] Which workers are next to be started
    next_workers = \() {
      self$available_workers |>
        head(length(self$next_ids))
    }
  ),
  private = list(
    .queue = NULL,
    .workers = NULL,
    .n_workers = NULL,
    verbosity_level = NULL,
    check_renv = NULL,
    track_files = NULL,
    out_formats = NULL,
    track_files_discards = NULL,
    track_files_keep = NULL,
    approved_pkgs_folder = NULL,
    approved_pkgs_url = NULL,
    log_dir = NULL
  )
)

wq_initialise <- function(
  self,
  private,
  n_workers,
  verbosity_level,
  check_renv,
  track_files,
  out_formats,
  track_files_discards,
  track_files_keep,
  approved_pkgs_folder,
  approved_pkgs_url,
  log_dir
) {
  private$check_renv <- check_renv
  private$verbosity_level <- verbosity_level
  private$track_files <- track_files
  private$out_formats <- out_formats
  private$track_files_discards <- track_files_discards
  private$track_files_keep <- track_files_keep
  private$approved_pkgs_folder <- approved_pkgs_folder
  private$approved_pkgs_url <- approved_pkgs_url
  private$log_dir <- log_dir

  private$.queue <- tibble::tibble(
    id = numeric(),
    tag = character(),
    script = character(),
    status = character(),
    result = list(),
    log_dir = character()
  )

  private$.workers <- tibble::tibble(
    id = seq_len(n_workers),
    session = vector(mode = "list", length = n_workers),
    id_script = numeric(n_workers),
    step = numeric(n_workers),
    active = FALSE
  )
}

wq_add_queue <- function(self, private, scripts, tag, status) {
  # Adding the log directory to the queue
  if (is.character(private$log_dir)) {
    # Check if the directory exists
    if (!file.exists(private$log_dir)) {
      cli::cli_abort(
        "Logs cannot be saved because {.val {private$log_dir}} does not exist"
      )
    }
    folder <- file.path(private$log_dir)
  } else {
    folder <- private$log_dir(scripts)
    # Check if the directory exists
    unique_folders <- unique(folder)
    if (any(!file.exists(unique_folders))) {
      missing <- unique_folders[!file.exists(unique_folders)]  # nolint: object_usage_linter
      cli::cli_abort(
        "Logs cannot be saved because {.val {missing}} does not exist"
      )
    }
  }

  private$.queue <- self$queue |>
    tibble::add_row(
      id = nrow(self$queue) + seq_along(scripts),
      tag = tag,
      script = scripts,
      status = status,
      log_dir = folder
    )
  return(invisible(self))
}

wq_push <- function(self, private, scripts, tag) {
  wq_add_queue(self, private, scripts, tag, status = "waiting")
}

wq_skip <- function(self, private, scripts, tag) {
  wq_add_queue(self, private, scripts, tag, status = "skipped")
}

wq_poll <- function(
  self,
  private,
  timeout,
  check_renv,
  verbosity_level,
  track_files,
  out_formats,
  track_files_discards,
  track_files_keep,
  approved_pkgs_folder,
  approved_pkgs_url,
  log_dir
) {
  # Start new sessions if there are available workers and waiting scripts in
  # the queue

  if (length(self$next_ids)) {
    nid <- self$next_ids
    wid <- self$next_workers
    private$.workers[["session"]][wid] <- replicate(
      n = length(wid),
      expr = whirl_r_session$new(
        check_renv = private$check_renv,
        verbosity_level = private$verbosity_level,
        track_files = private$track_files,
        out_formats = private$out_formats,
        track_files_discards = private$track_files_discards,
        track_files_keep = private$track_files_keep,
        approved_pkgs_folder = private$approved_pkgs_folder,
        approved_pkgs_url = private$approved_pkgs_url,
        log_dir = private$log_dir
      ),
      simplify = FALSE
    )
    private$.workers[wid, "id_script"] <- nid
    private$.workers[wid, "active"] <- TRUE
    private$.queue[nid, "status"] <- "running"
  }

  # Check for active sessions that are idle and start the next step if needed
  # When completed the session is stopped and the status in the queue is updated

  i_active <- which(private$.workers$active)
  i_timeout <- round(timeout / length(i_active))
  for (i in i_active) {
    p <- private$.workers$session[[i]]$poll(timeout = i_timeout)
    if (p == "ready") private$.workers$session[[i]]$read()
    if (private$.workers$session[[i]]$get_state() == "idle") {
      wq_next_step(self, private, i)
    }
  }

  return(self$queue$status)
}

wq_wait <- function(self, private, timeout) {
  start <- Sys.time()
  timeout <- timeout / 1000 # Convert to secs
  go <- TRUE
  while (go) {
    self$poll(50)
    go <- any(self$queue$status %in% c("waiting", "running"))
    if (timeout >= 0 && difftime(Sys.time(), start, units = "secs") > timeout) {
      break
    }
  }
  return(invisible(self))
}

wq_next_step <- function(self, private, wid) {
  private$.workers$step[[wid]] <- private$.workers$step[[wid]] + 1
  id_script <- private$.workers$id_script[[wid]]
  session <- private$.workers$session[[wid]]

  switch(
    EXPR = private$.workers$step[[wid]],

    # Step 1: Log script
    "1" = {
      script <- private$.queue$script[[id_script]]
      session$log_script(script)
    },
    # Step 2: Create log
    "2" = {
      session$create_log()
    },
    # Step 3: Finish log and create outputs
    "3" = {
      private$.queue$result[[id_script]] <-
        session$log_finish()$create_outputs(
          out_dir = private$.queue$log_dir[[id_script]],
          format = private$out_formats
        )
      # fmt: skip
      private$.queue$status[[id_script]] <-
        private$.queue$result[[id_script]]$status$status

      private$.workers$session[wid] <- list(NULL)
      private$.workers$active[[wid]] <- FALSE
      private$.workers$id_script[[wid]] <- 0
      private$.workers$step[[wid]] <- 0
    }
  )

  return(invisible(wid))
}

wq_run <- function(scripts, self) {
  self$push(scripts)$wait()
  on.exit(gc()) # finalizes used whirl_r_sessions - cleanup temp folders
}
