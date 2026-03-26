# Options for whirl

### verbosity_level

Verbosity level for functions in whirl. See
[zephyr::verbosity_level](https://novonordisk-opensource.github.io/zephyr/reference/verbosity_level.html)
for details.

- Default: `NA_character_`

- Option: `whirl.verbosity_level`

- Environment: `R_WHIRL_VERBOSITY_LEVEL`

### out_formats

Which log format(s) to produce. Possibilities are `html`, `json`, and
markdown formats: `gfm`, `commonmark`, and `markua`.

- Default: `"html"`

- Option: `whirl.out_formats`

- Environment: `R_WHIRL_OUT_FORMATS`

### track_files

Should files read and written be tracked? Currently only supported on
Linux.

- Default: `FALSE`

- Option: `whirl.track_files`

- Environment: `R_WHIRL_TRACK_FILES`

### check_renv

Should the projects renv status be checked?

- Default: `FALSE`

- Option: `whirl.check_renv`

- Environment: `R_WHIRL_CHECK_RENV`

### track_files_discards

List of file naming patterns not be tracked when track_files = TRUE

- Default:
  `c("^/lib", "^/etc", "^/lib64", "^/usr", "^/var", "^/opt", "^/sys", "^/proc", "^/tmp", "^/null", "^/urandom", "^/.cache")`

- Option: `whirl.track_files_discards`

- Environment: `R_WHIRL_TRACK_FILES_DISCARDS`

### track_files_keep

List of file naming patterns always to be tracked when track_files =
TRUE

- Default: `NULL`

- Option: `whirl.track_files_keep`

- Environment: `R_WHIRL_TRACK_FILES_KEEP`

### approved_packages

List of approved R packages and their version in the format:
{name}@{version}

- Default: `NULL`

- Option: `whirl.approved_packages`

- Environment: `R_WHIRL_APPROVED_PACKAGES`

### approved_python_packages

List of approved Python packages and their version in the format:
{name}@{version}

- Default: `NULL`

- Option: `whirl.approved_python_packages`

- Environment: `R_WHIRL_APPROVED_PYTHON_PACKAGES`

### n_workers

Number of simultaneous workers used in the run function. A maximum of
128 workers is allowed.

- Default: `1`

- Option: `whirl.n_workers`

- Environment: `R_WHIRL_N_WORKERS`

### log_dir

The output directory of the log files. Default is the folder of the
executed script. log_dir can be a path as a character or it can be a
function that takes the script path as input and returns the log
directory. For more information see the examples of
[`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
or
[`vignette('whirl')`](https://novonordisk-opensource.github.io/whirl/articles/whirl.md).

- Default: `function (x) dirname(x)`

- Option: `whirl.log_dir`

- Environment: `R_WHIRL_LOG_DIR`

### execute_dir

The working directory of the process executing each script. Default us
to execute R files from the working directory when calling
[`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
and all other functions from the directory of the script. To change
provide a character path (used for all scripts) or a function that takes
the script as input and returns the execution directory.

- Default: `NULL`

- Option: `whirl.execute_dir`

- Environment: `R_WHIRL_EXECUTE_DIR`

### wait_timeout

Timeout for waiting for the R process from callr::r_session to start, in
milliseconds.

- Default: `9000`

- Option: `whirl.wait_timeout`

- Environment: `R_WHIRL_WAIT_TIMEOUT`

### environment_secrets

Secret environment variable patterns. Any variables matching will not be
included in the logs.

- Default: `c("BASH_FUNC", "_SSL_CERT", "_KEY", "_PAT", "_TOKEN")`

- Option: `whirl.environment_secrets`

- Environment: `R_WHIRL_ENVIRONMENT_SECRETS`

### with_options

List of options to set in the child sessions executing the scripts.

- Default: [`list()`](https://rdrr.io/r/base/list.html)

- Option: `whirl.with_options`

- Environment: `R_WHIRL_WITH_OPTIONS`
