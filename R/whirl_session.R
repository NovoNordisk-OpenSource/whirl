
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
