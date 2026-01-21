# Changelog

## whirl 0.3.2

- Fixed bug where warnings were given when scripts are missing a final
  EOL
  ([\#206](https://github.com/NovoNordisk-OpenSource/whirl/issues/206))
- Fixed bug so no warning is shown when saving
  [`options()`](https://rdrr.io/r/base/options.html) to the temp folder
  used by whirl
  ([\#206](https://github.com/NovoNordisk-OpenSource/whirl/issues/206))
- Fixed so Quarto is started with the right renv library paths when
  using renv
  ([\#215](https://github.com/NovoNordisk-OpenSource/whirl/issues/215))
- Refactored log.qmd to remove read_log.
- Added new `with_options` argument to
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).
  This is the only options being set in the child sessions now.
  ([\#187](https://github.com/NovoNordisk-OpenSource/whirl/issues/187))

## whirl 0.3.1

CRAN release: 2025-08-25

- Improved error handling when the log cannot be created.
- Added new option `environment_secrets` to control which secret
  environment variables not to include in the log.
- Improved progress bar of
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  to show all currently running scripts.
- Removed `verbosity_level` argument to
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  since it is now completely controlled by zephyr options (see
  [`help("whirl-options")`](https://novonordisk-opensource.github.io/whirl/reference/whirl-options.md)).
- Fixed bug where a script would not execute if using
  `options(warn = 2)`
  ([\#151](https://github.com/NovoNordisk-OpenSource/whirl/issues/151))
- The log now distinguishes between directly and indirectly used
  packages, and visually highlights their approval status if a list of
  approved packages is provided.
- Added check for approved Python packages with the
  `approved_python_packages` option similar to for R.
- Improved how to find used Python packages, so it now only lists the
  packages actually used in the script.

## whirl 0.3.0

CRAN release: 2025-07-08

- Add
  [`write_biocompute()`](https://novonordisk-opensource.github.io/whirl/reference/write_biocompute.md)
  to create [BioCompute Objects](https://www.biocomputeobject.org/)
  containing the logs in a standardized JSON format.
- Calling
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  with `track_files = TRUE` now checks if strace can be attached to the
  process.
- Improved json logs and similar returned output from running a script
  (`result` column in return from
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)).
- Simplified approved packages check. Now the user supplies a character
  vector of packages and versions specified as `{package}@{version}`.
- Implement use of `tag` in the returned output. Each script is now
  tagged with the step name in the summary report.

## whirl 0.2.0

CRAN release: 2025-04-15

- Initial CRAN release.
- Default `input` argument of
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  set to “\_whirl.yml”.
- Added RStudio addins for running all scripts and the active script.
- Fix typo in `track_files_discards` option.
- Increases unit test coverage and skips
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  tests etc. when Quarto is not available.

## whirl 0.1.7

- Enable redirection of logs through the `log_dir` argument in
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).
- Changed the title on the individual logs to the script name and moved
  the path to a distinct section within the title-block.
- Fixed a bug where the hyperlink in the summary files was not rendered
  correctly.
- Fixed a bug where the installed python packages were not listed in the
  log.
- Enable the use of R expressions in the yaml configuration file.
- Enables the user to define the working directory for each script with
  the `execute_dir` option.

## whirl 0.1.6

- Added support for logging of Python scripts with
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).
- Improved unit tests for
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).
- Fixing a bug where the queue was not returned correctly in some
  instances.
- Switched to using [`Sys.glob()`](https://rdrr.io/r/base/Sys.glob.html)
  instead of [`utils::glob2rx()`](https://rdrr.io/r/utils/glob2rx.html).

## whirl 0.1.4

- Add
  [`use_whirl()`](https://novonordisk-opensource.github.io/whirl/reference/use_whirl.md)
  utility function.

## whirl 0.1.3

- Adding additional arguments to
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  allowing the user to:
  - control the verbosity level
  - specify whether renv should be checked
  - specify which files to track
  - adjust the output format of the log files.

## whirl 0.1.1

- Fix enabling rendering of md log formats(“gfm”, “commonmark”,
  “markua”).

## whirl 0.1.0

- First version publicly available on GitHub.

## whirl 0.0.5

- Updated documentation
- README and vignettes are now ready for users.

## whirl 0.0.4

- Adjusting
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
  to unify execution of scripts, lists of scripts, and configuration
  files.
- Using multiple independent
  [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
  when executing several scripts.
- Cleanup of namespace and exported functions.

## whirl 0.0.3

- Initial version of
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).
- Substituting spinner with progress bar when executing single scripts.

## whirl 0.0.2

- Update so that the execution (including order of execution) can be
  controlled through a config file.

## whirl 0.0.1

- First version of package.
