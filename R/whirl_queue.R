#' private$.queue for logging several scripts
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
    poll = \() {
      wq_poll(self, private)
    },
    wait = \(timeout = -1) {
      wq_wait(self, private, timeout)
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

wq_poll <- function(self, private) {

  if (length(self$next_ids)) {
    nid <- self$next_ids
    wid <- self$next_workers
    private$.workers[wid, "id_script"] <- nid
    private$.workers[wid, "active"] <- TRUE
    private$.queue[nid, "status"] <- "running"
  }

  return(self$queue$status)
}

wq_wait <- function(timeout, self, private) {
  start <- Sys.time()
  timeout <- timeout / 1000 # Convert to secs
  go <- TRUE
  while (go) {
    self$poll()
    go <- self$n_waiting | self$n_active
    if (timeout >= 0 && difftime(Sys.time(), start, units = "secs") > timeout) break
  }
  return(invisible(self))
}

wq_next_step <- function(ids, self, private) {

  for (i in ids) {
    # Increment the step before executing it
    purrr::pluck(private$.queue, "step", i) <- purrr::pluck(private$.queue, "step", i) + 1

    switch (EXPR = purrr::pluck(private$.queue, "step", i),
            # Step 1: Start session
            "1" = {
              purrr::pluck(private$.queue, "active", i) <- TRUE
              purrr::pluck(private$.queue, "session", i) <- whirl_r_session$new()
            },
            # Step 2: Log script
            "2" = {
              purrr::pluck(private$.queue, "session", i)$
                log_script(purrr::pluck(private$.queue, "script", i))$
                wait()$
                check_status()
            },
            # Step 3: Create log
            "3" = {
              purrr::pluck(private$.queue, "session", i)$
                create_log()$
                wait()$
                check_status()
            },
            # Step 4: Finish log and create outputs
            "4" = {
              purrr::pluck(private$.queue, "result", i) <- purrr::pluck(private$.queue, "session", i)$
                log_finish()$
                create_outputs(out_dir = getwd(), format = "html")
            },
            # Step 5: Stop session
            "5" = {
              purrr::pluck(private$.queue, "session", i)$
                kill()
              purrr::pluck(private$.queue, "active", i) <- FALSE
              purrr::pluck(private$.queue, "status", i) <- purrr::pluck(private$.queue, "result", i, "status", "status")
            }
    )
  }
  return(ids)
}

