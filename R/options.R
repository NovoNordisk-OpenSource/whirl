#' Whirl Package Options
#'
#' @description
#' Internally used, package-specific options for whirl. All options will prioritize
#' R options() values, and fall back to environment variables if undefined. If
#' neither the option nor the environment variable is set, a default value is used.
#'
#' @name whirl-options
#'
#' @section Checking Option Values:
#' Option values specific to `whirl` can be accessed by passing the package name to `env`.
#'
#' ```r
#' zephyr::opts_pkg(env = "whirl")
#' zephyr::opt_pkg(x, default, env = "whirl")
#' ```
#'
#' @seealso [options()], [getOption()], [Sys.setenv()], [Sys.getenv()]
#'
#' @eval paste(zephyr::as_roxygen_docs_pkg("whirl"), collapse = "\n")
NULL

#' Internal reuse of options description
#' @name options_params
#' @keywords internal
#'
#' @eval paste(zephyr::as_params_pkg("whirl"), collapse = "\n")
NULL

zephyr::define_option_pkg(
  option = "out_formats",
  default = "html",
  desc = "Which log format(s) to produce. Possiblities are `html`, `json`, and markdown formats:`gfm`, `commonmark`, and `markua`."
)

zephyr::define_option_pkg(
  option = "track_files",
  default = FALSE,
  desc = "Should files read and written be tracked? Currently only supported on Linux.",
  envvar_fn = zephyr::envvar_is_true_pkg()
)

zephyr::define_option_pkg(
  option = "check_renv",
  default = FALSE,
  desc = "Should the projects renv status be checked?",
  envvar_fn = zephyr::envvar_is_true_pkg()
)

zephyr::define_option_pkg(
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
    # "^/renv"#,
    .libPaths()
  ),
  desc = "List of file naming patterns not be tracked when track_files = TRUE",
  envvar_fn = zephyr::envvar_str_split_pkg(delim = ";")
)

zephyr::define_option_pkg(
  option = "track_files_keep",
  default = paste0("^", getwd()),
  desc = "List of file naming patterns alway to be tracked when track_files = TRUE",
  envvar_fn = zephyr::envvar_str_split_pkg(delim = ";")
)

zephyr::define_option_pkg(
  option = "verbosity_level",
  default = "verbose",
  desc = "How chatty should the log be? Possibilities are `quiet`, `minimal` and `verbose`."
)

zephyr::define_option_pkg(
  option = "approved_pkgs_folder",
  default = NULL,
  desc = "Approved folder library packages",
  envvar_fn = zephyr::envvar_str_split_pkg(delim = ";")
)

zephyr::define_option_pkg(
  option = "approved_pkgs_url",
  default = NULL,
  desc = "Approved URL library packages",
  envvar_fn = zephyr::envvar_str_split_pkg(delim = ";")
)

zephyr::define_option_pkg(
  option = "n_workers",
  default = 1,
  desc = "Number of simultanous workers used in the run function. A maximum of 128 workers is allowed."
)
