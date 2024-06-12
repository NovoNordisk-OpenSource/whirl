
whirl_r_session <- R6::R6Class(

  classname = "whirl_r_session",

  public = list(

    initialize = \(lib_paths = .libPaths(), verbosity_level = zephyr::get_opt("verbosity_level")) {

      super$initialize()

      private$dir <- withr::local_tempdir(clean = FALSE)
      private$dummy_qmd <- file.path(private$dir, "dummy.qmd")
      private$log_qmd <- file.path(private$dir, "log.qmd")
      private$strace_log <- file.path(private$dir, "strace_log")
      private$lib_paths <- lib_paths
      private$verbosity_level <- verbosity_level

      file.copy(from = system.file("documents/dummy.qmd", package = "whirl"), to = private$dummy_qmd)
      file.copy(from = system.file("documents/log.qmd", package = "whirl"), to = private$log_qmd)
      },

    finalize = \() {
      unlink(private$dir, recursive = TRUE)
      private$spinner$finish()
      super$finalize()
      },

    print = \() {
      cat("whirl_r_session object\n")
      super$print()
      cat("  Directory: ", private$dir, "\n")
      cat("  Dummy QMD: ", private$dummy_qmd, "\n")
      cat("  Log QMD: ", private$log_qmd, "\n")
      },

    get_dir = \() {private$dir},

    start_strace = \() {start_strace(pid = super$get_pid(), file = private$strace_log)},

    run_something = \() for (i in seq(100)) {Sys.sleep(0.1); private$spinner$spin()}

  ),

  private = list(
    dir = NULL,
    dummy_qmd = NULL,
    log_qmd = NULL,
    strace_log = NULL,
    lib_paths = NULL,
    verbosity_level = NULL,
    spinner = cli::make_spinner()
  ),

  inherit = callr::r_session
)

p <- whirl_r_session$new(verbosity_level = "a")

p

a <- p$get_dir() |> print()

dir.exists(a)
a |> list.files()

p$run_something()


p$kill()
p$finalize()
rm(p)
gc()
dir.exists(a)


# Pseudo code for whirl_r_session

whirl_r_session <- R6::R6Class(
  classname = "whirl_r_session",
  public = list(
    set_inputs = \(...) private$script <- script,
    start_strace = \() super$get_pid() |> print(),
    set_verbosity_level = \(level) private$verbosity_level <- level,
    set_dir = \(dir) private$dir <- dir,
    render_script = \() TRUE,
    read_strace = \() TRUE,
    render_log = \() TRUE,
    report_status = \() TRUE,
    create_outputs = \(formats) TRUE
  ),
  private = list(
    script = NULL,
    verbosity_level = "verbose",
    dir = NULL,
    last_status = NULL,
    spinner = NULL
  ),
  inherit = callr::r_session
)

# Pseudo use inside run_script()

p <- whirl_r_session$new()
on.exit(p$kill())

whirl_r_session

p

p$set_inputs()
p$set_dir()
p$set_verbosity_level()
p$start_strace()
p$render_script()
p$read_strace()
p$render_log()
p$report_status()
p$create_outputs()

p$call(\() Sys.sleep(10))
p$wait(timeout = 1)
p$read()


p <- whirl:::whirl_r_session$new()
