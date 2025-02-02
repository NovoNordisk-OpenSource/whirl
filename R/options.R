#' @eval options::as_roxygen_docs()
NULL

#' Internal reuse of options description
#' @eval options::as_params()
#' @name options_params
#' @keywords internal
#'
NULL

options::define_option(
  option = "out_formats",
  default = "html",
  desc = "Which log format(s) to produce. Possibilities are `html`, `json`, and
  markdown formats: `gfm`, `commonmark`, and `markua`."
)

options::define_option(
  option = "track_files",
  default = FALSE,
  desc = "Should files read and written be tracked?
  Currently only supported on Linux.",
  envvar_fn = options::envvar_is_true()
)

options::define_option(
  option = "check_renv",
  default = FALSE,
  desc = "Should the projects renv status be checked?",
  envvar_fn = options::envvar_is_true()
)

options::define_option(
  option = "track_files_discards",
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
    "^/.cache",
    .libPaths()
  ),
  desc = "List of file naming patterns not be tracked when track_files = TRUE",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "track_files_keep",
  default = paste0("^", getwd()),
  desc = "List of file naming patterns always to be tracked when
  track_files = TRUE",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "verbosity_level",
  default = "verbose",
  desc = "How chatty should the log be? Possibilities are
  `quiet`, `minimal` and `verbose`."
)

options::define_option(
  option = "approved_pkgs_folder",
  default = NULL,
  desc = "Approved folder library packages",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "approved_pkgs_url",
  default = NULL,
  desc = "Approved URL library packages",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "n_workers",
  default = 1,
  desc = "Number of simultaneous workers used in the run function.
  A maximum of 128 workers is allowed."
)

options::define_option(
  option = "log_dir",
  default = dirname,
  desc = "The output directory of the log files. Default is the folder of the
  executed script. log_dir can be a path as a character or it can be a function
  that takes the script path as input and returns the log directory.
  For more information see the examples of `run()` or `vignette('whirl')`."
)

options::define_option(
  option = "execute_dir",
  default = NULL,
  desc = "The working directory of the process executing each script.
  Default us to execute R files from the working directory when calling `run()`
   and all other functions from the directory of the script. To change provide
  a character path (used for all scripts) or a function that takes the script
  as input and returns the execution directory."
)
