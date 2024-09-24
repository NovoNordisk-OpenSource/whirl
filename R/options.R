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
  desc = "Which log format(s) to produce. Possiblities are `html`, `json`, and markdown formats:`gfm`, `commonmark`, and `markua`. See also details."
)

options::define_option(
  option = "track_files",
  default = FALSE,
  desc = "Should files read and written be tracked? Currently only supported on Linux.",
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
    # "^/renv"#,
    .libPaths()
  ),
  desc = "List of file naming patterns not be tracked when track_files = TRUE",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "track_files_keep",
  default = paste0("^", getwd()),
  desc = "List of file naming patterns alway to be tracked when track_files = TRUE",
  envvar_fn = options::envvar_str_split(delim = ";")
)

options::define_option(
  option = "verbosity_level",
  default = "verbose",
  desc = "How chatty should the log be? Possibilities are `quiet`, `verbose`, and `debug`."
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
  desc = "Number of simultanous workers used in the run function"
)
