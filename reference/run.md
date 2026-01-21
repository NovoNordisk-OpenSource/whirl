# Execute single or multiple R, R Markdown, and Quarto scripts

Executes and logs the execution of the scripts. Logs for each script are
stored in the same folder as the script.

The way the execution is logged is configurable through several options
for e.g. the verbosity of the logs. See
[whirl-options](https://novonordisk-opensource.github.io/whirl/reference/whirl-options.md)
on how to configure these.

## Usage

``` r
run(
  input = "_whirl.yml",
  steps = NULL,
  summary_file = "summary.html",
  n_workers = zephyr::get_option("n_workers", "whirl"),
  check_renv = zephyr::get_option("check_renv", "whirl"),
  track_files = zephyr::get_option("track_files", "whirl"),
  out_formats = zephyr::get_option("out_formats", "whirl"),
  log_dir = zephyr::get_option("log_dir", "whirl"),
  with_options = zephyr::get_option("with_options", "whirl")
)
```

## Arguments

- input:

  A character vector of file path(s) to R, R Markdown, Quarto scripts,
  or files in a folder using regular expression, or to to a whirl config
  file. The input can also be structured in a list where each element
  will be executed sequentially, while scripts within each element can
  be executed in parallel.

- steps:

  An optional argument that can be used if only certain steps within a
  config files (or list) is to be executed. Should be equivalent to the
  names of the steps found in the config file. If kept as NULL (default)
  then all steps listed in the config file will be executed.

- summary_file:

  A character string specifying the file path where the summary log will
  be stored.

- n_workers:

  Number of simultaneous workers used in the run function. A maximum of
  128 workers is allowed.. Default: `1`.

- check_renv:

  Should the projects renv status be checked?. Default: `FALSE`.

- track_files:

  Should files read and written be tracked? Currently only supported on
  Linux.. Default: `FALSE`.

- out_formats:

  Which log format(s) to produce. Possibilities are `html`, `json`, and
  markdown formats: `gfm`, `commonmark`, and `markua`.. Default:
  `"html"`.

- log_dir:

  The output directory of the log files. Default is the folder of the
  executed script. log_dir can be a path as a character or it can be a
  function that takes the script path as input and returns the log
  directory. For more information see the examples of `run()` or
  [`vignette('whirl')`](https://novonordisk-opensource.github.io/whirl/articles/whirl.md)..
  Default: `function (x) dirname(x)`.

- with_options:

  List of options to set in the child sessions executing the scripts..
  Default: [`list()`](https://rdrr.io/r/base/list.html).

## Value

A tibble containing the execution results for all the scripts.

## Examples

``` r
if (FALSE) {
# Copy example scripts:
file.copy(
  from = system.file("examples", c("success.R", "warning.R", "error.R"),
    package = "whirl"
  ),
  to = tempdir()
)

# Run a single script and create log:
run(file.path(tempdir(), "success.R"))

# Run several scripts in parallel on up to 2 workers:
run(
  input = file.path(tempdir(), c("success.R", "warning.R", "error.R")),
  n_workers = 2
)

# Run several scripts in two steps by providing them as list elements:
run(
  list(
    file.path(tempdir(), c("success.R", "warning.R")),
    file.path(tempdir(), "error.R")
  )
)

# Re-directing the logs to a sub-folder by utilizing the log_dir argument in
# run(). This will require that the sub-folder exists.

# Specifying the path using a manually defined character
run(file.path(tempdir(), "success.R"), log_dir = tempdir())

# Specifying the path with a generic function that can handle the scripts
# individually.
run(
  input = file.path(tempdir(), "success.R"),
  log_dir = function(x) {paste0(dirname(x), "/logs")}
)
}
```
