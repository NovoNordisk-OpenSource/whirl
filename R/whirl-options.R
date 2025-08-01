#' @name whirl-options
#' @title Options for whirl
#' @description
#' `r zephyr::list_options(as = "markdown", .envir = "whirl")`
NULL

#' @title Internal parameters for reuse in functions
#' @name whirl-options-params
#' @eval zephyr::list_options(as = "params", .envir = "whirl")
#' @details
#' See [whirl-options] for more information.
#' @keywords internal
NULL

zephyr::create_option(
  name = "verbosity_level",
  default = NA_character_,
  description = "Verbosity level for functions in whirl.
  See [zephyr::verbosity_level] for details."
)

zephyr::create_option(
  name = "out_formats",
  default = "html",
  description = "Which log format(s) to produce. Possibilities are `html`,
  `json`, and markdown formats: `gfm`, `commonmark`, and `markua`."
)

zephyr::create_option(
  name = "track_files",
  default = FALSE,
  description = "Should files read and written be tracked?
  Currently only supported on Linux."
)

zephyr::create_option(
  name = "check_renv",
  default = FALSE,
  description = "Should the projects renv status be checked?"
)

zephyr::create_option(
  name = "track_files_discards",
  default = c(
    "^/lib",
    "^/etc",
    "^/lib64",
    "^/usr",
    "^/var",
    "^/opt",
    "^/sys",
    "^/proc",
    "^/tmp",
    "^/null",
    "^/urandom",
    "^/.cache"
  ),
  description = "List of file naming patterns not be tracked when track_files = TRUE"
)

zephyr::create_option(
  name = "track_files_keep",
  default = NULL,
  description = "List of file naming patterns always to be tracked when
  track_files = TRUE"
)

zephyr::create_option(
  name = "approved_packages",
  default = NULL,
  description = "List of approved packages and their version in the format: \\{name\\}@\\{version\\}"
)

zephyr::create_option(
  name = "n_workers",
  default = 1,
  description = "Number of simultaneous workers used in the run function.
  A maximum of 128 workers is allowed."
)

zephyr::create_option(
  name = "log_dir",
  default = \(x) dirname(x),
  description = "The output directory of the log files. Default is the folder of
  the executed script. log_dir can be a path as a character or it can be a
  function that takes the script path as input and returns the log directory.
  For more information see the examples of `run()` or `vignette('whirl')`."
)

zephyr::create_option(
  name = "execute_dir",
  default = NULL,
  description = "The working directory of the process executing each script.
  Default us to execute R files from the working directory when calling `run()`
   and all other functions from the directory of the script. To change provide
  a character path (used for all scripts) or a function that takes the script
  as input and returns the execution directory."
)

zephyr::create_option(
  name = "wait_timeout",
  default = 9000,
  description = "Timeout for waiting for the R process from callr::r_session to 
  start, in milliseconds."
)

zephyr::create_option(
  name = "environment_secrets",
  default = c(
    "BASH_FUNC",
    "_SSL_CERT",
    "_KEY",
    "_PAT",
    "_TOKEN"
  ),
  description = "Secret environment variable patterns. 
  Any variables matching will not be included in the logs."
)
