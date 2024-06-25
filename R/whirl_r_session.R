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
    run = \(func, args = list(), package = FALSE) {
      wrs_run(self, private, super, func, args, package)
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
    spinner = NULL
  ),

  inherit = callr::r_session
)

wrs_init <- function(self, private, super, lib_paths, verbosity) {
  super$initialize()

  # TODO: Is there a way to use `.local_envir` to avoid having to clean up the temp dir in finalize?
  private$dir <- withr::local_tempdir(clean = FALSE)
  private$lib_paths <- lib_paths
  private$verbosity <- verbosity

  if (private$verbosity > 0) {
    private$spinner <- cli::make_spinner()
  }

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

wrs_run <- function(self, private, super, func, args, package) {
  super$call(func = func, args = args, package = package)
  go = TRUE
  while (go) {
    private$spinner$spin(template = "{spin} Running...")
    if (super$poll_process(timeout = 100) != "timeout") go = FALSE
  }
  private$spinner$finish()
  super$read() |>
    get_result()
}

get_result <- function(status, env = parent.frame()) {
  if (!is.null(status$error)) {
    status$error |>
      as.character() |>
      rlang::abort(call = env)
  }
  status$result
}
