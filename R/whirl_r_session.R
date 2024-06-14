#' Internal whirl extention of the callr::r_session class to be used in `run_script()`
#' @export

whirl_r_session <- R6::R6Class(

  classname = "whirl_r_session",

  public = list(

    #' @description
    #' Initialize a new whirl R session
    #' @param lib_paths character vector of library paths
    #' @return An `whirl_r_session` object, that inherits from `callr::r_session`
    initialize = \(lib_paths = .libPaths(), verbosity = zephyr::get_opt("verbosity_level")) {
      wrs_init(self, private, super, lib_paths, verbosity)
      },

    #' @description
    #' Finalize the R session, and clean up temp files etc.
    finalize = \() {
      wrs_finalize(self, private, super)
      },

    #' @description
    #' Print the whirl R session object
    #' @return [invisible] self
    print = \() { # For easier debugging
      wrs_print(self, private, super)
      },

    #' @description
    #' Run an expression with strace and spinner
    #'
    wrun = \(func, args = list(), package = FALSE) {
      wrs_run_with_output_spinner(self, private, func, args, package)
      },

    #' @description Get the directory of the whirl R session
    get_dir = \() {private$dir},

    #' @description Start strace
    use_strace = \() {
      start_strace(
        pid = super$get_pid(),
        file = file.path(private$dir, private$strace_log)
        )
      },

    #' @description Get strace
    get_strace = \() read_strace_info(),

    #' @description Run
    run_something = \() for (i in seq(100)) {Sys.sleep(0.1); private$spinner$spin()}

  ),

  private = list(
    dir = NULL,
    dummy_qmd = "dummy.qmd",
    log_qmd = "log.qmd",
    strace_log = "strace.log",
    lib_paths = NULL,
    verbosity = NULL,
    spinner = cli::make_spinner()
  ),

  inherit = callr::r_session
)

wrs_init <- function(self, private, super, lib_paths, verbosity) {
  super$initialize()

  private$dir <- withr::local_tempdir(clean = FALSE)
  private$lib_paths <- lib_paths
  private$verbosity <- verbosity

  file.copy(from = system.file("documents/dummy.qmd", package = "whirl"), to = private$dummy_qmd)
  file.copy(from = system.file("documents/log.qmd", package = "whirl"), to = private$log_qmd)
}

wrs_finalize <- function(self, private, super) {
  unlink(private$dir, recursive = TRUE)
  private$spinner$finish()
  super$finalize()
}

wrs_print <- function(self, private, super) {

  msg <- c(
    capture.output(super$print()),
    "Directory: {private$dir}",
    "Verbosity: {private$verbosity}"
    ) |>
    rlang::set_names("*")

  cli::cli_bullets(
    c(
      "<whirl_r_session> object",
      msg
    )
  )
  return(invisible(self))
}

# From: https://github.com/r-lib/callr/blob/main/R/r-session.R

wrs_run_with_output_spinner <- function(self, private, func, args, package) {
  self$call(func, args, package)

  go <- TRUE
  res <- NULL

  while (go) {
    ## TODO: why is this in a tryCatch?
    res <- tryCatch(
      { processx::poll(list(private$pipe), -1)
        msg <- self$read()
        if (is.null(msg)) {
          private$spinner$spin()
          next
          }
        if (msg$code == 200 || (msg$code >= 500 && msg$code < 600)) {
          return(msg)
        }
        if (msg$code == 301) {
          rs__handle_condition(msg$message)
        }
      },
      interrupt = function(e) {
        self$interrupt()
        ## The R process will catch the interrupt, and then save the
        ## error object to a file, but this might still take some time,
        ## so we need to poll here. If the bg process ignores
        ## interrupts, then we kill it.
        ps <- processx::poll(list(private$pipe), 1000)[[1]]
        if (ps == "timeout") {
          self$kill()
        } else {
          res <<- self$read()
          go <<- FALSE
        }
        iconn <- structure(
          list(message = "Interrupted"),
          class = c("interrupt", "condition"))
        signalCondition(iconn)
        cat("\n")
        invokeRestart("abort")
      })
  }
  res
}

