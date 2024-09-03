#' queue for logging several scripts
#' @noRd

whirl_queue <- R6::R6Class(
  classname = "whirl_queue",
  public = list(
    initialize = \(n_workers = 1) {
      wq_initialise(self, private, n_workers)
    },
    push = \(scripts) {
      wq_push(self, private, scripts)
    },
    poll = \(timeout) {
      wq_poll(self, private, timeout)
    },
    wait = \(timeout = -1) {
      wq_wait(self, private, timeout)
    },
    run = \(scripts) {
      wq_run(scripts, self)
    },
    print = \() {
      print(self$queue)
      return(invisible(self))
    }
  ),

  active = list(
    queue = \() {
      private$.queue
    },
    workers = \() {
      private$.workers
    },
    available_workers = \() {
      which(!self$workers$active)
    },
    next_ids = \() {
      self$queue$id[self$queue$status == "waiting"] |>
        head(length(self$available_workers))
    },
    next_workers = \() {
      self$available_workers |>
        head(length(self$next_ids))
    }
  ),

  private = list(
    .queue = NULL,
    .workers = NULL,
    .n_workers = NULL
  )
)

wq_initialise <- function(self, private, n_workers) {
  private$.queue <- tibble::tibble(
    id = numeric(),
    script = character(),
    status = character(),
    result = list()
  )

  private$.workers <- tibble::tibble(
    id = seq_len(n_workers),
    session = vector(mode = "list", length = n_workers),
    id_script = numeric(n_workers),
    step = numeric(n_workers),
    active = FALSE
  )
}

wq_push <- function(self, private, scripts) {
  private$.queue <- self$queue |>
    tibble::add_row(
      id = nrow(self$queue) + seq_along(scripts),
      script = scripts,
      status = "waiting"
    )
  return(invisible(self))
}

wq_poll <- function(self, private, timeout) {

  # Start new sessions if there are available workers and waiting scripts in the queue

  if (length(self$next_ids)) {
    nid <- self$next_ids
    wid <- self$next_workers
    private$.workers[["session"]][wid] <- replicate(n = length(wid), expr = whirl_r_session$new(verbose = TRUE), simplify = FALSE)
    private$.workers[wid, "id_script"] <- nid
    private$.workers[wid, "active"] <- TRUE
    private$.queue[nid, "status"] <- "running"
  }

  # Check for active sessions that are idle and start the next step if needed
  # When completed the session is stopped and the status in the queue is updated

  i_active <- which(private$.workers$active)
  i_timeout <- round(timeout/length(i_active))
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
    if (timeout >= 0 && difftime(Sys.time(), start, units = "secs") > timeout) break
  }
  return(invisible(self))
}

wq_next_step <- function(self, private, wid) {

  purrr::pluck(private$.workers, "step", wid) <- purrr::pluck(private$.workers, "step", wid) + 1
  id_script <- purrr::pluck(private$.workers, "id_script", wid)
  session <- purrr::pluck(private$.workers, "session", wid)

  switch (EXPR = purrr::pluck(private$.workers, "step", wid),

          # Step 1: Log script
          "1" = {
            script <- purrr::pluck(private$.queue, "script", id_script)
            session$log_script(script)
          },
          # Step 2: Create log
          "2" = {
            session$create_log()
          },
          # Step 3: Finish log and create outputs
          "3" = {
            purrr::pluck(private$.queue, "result", id_script) <- session$
              log_finish()$
              create_outputs(out_dir = getwd(), format = "html")

            purrr::pluck(private$.queue, "status", id_script) <-
              purrr::pluck(private$.queue, "result", id_script, "status", "status")

            session$finalize()

            purrr::pluck(private$.workers, "session", wid) <- NULL
            purrr::pluck(private$.workers, "active", wid) <- FALSE
            purrr::pluck(private$.workers, "id_script", wid) <- 0
            purrr::pluck(private$.workers, "step", wid) <- 0
          }
  )

  return(invisible(wid))
}

wq_run <- function(scripts, self) {
  UseMethod("wq_run")
}

wq_run.default <- function(scripts, self) {
  stop("Scripts can only be character or numeric")
}

wq_run.character <- function(scripts, queue) {
  queue$
    push(scripts)$
    wait()
}

wq_run.list <- function(scripts, queue) {
  for (i in seq_along(scripts)) {
    queue$run(scripts[[i]])
  }
  return(invisible(queue))
}

