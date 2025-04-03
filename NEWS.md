# whirl 0.2.0

* Initial CRAN release.
* Default `input` argument of `run()` set to "_whirl.yml".
* Added RStudio addins for running all scripts and the active script.
* Fix typo in `track_files_discards` option.
* Increases unit test coverage and skips `run()` tests etc. when Quarto is not available.

# whirl 0.1.7
* Enable redirection of logs through the `log_dir` argument in `run()`.
* Changed the title on the individual logs to the script name and moved the path to a distinct section within the title-block.
* Fixed a bug where the hyperlink in the summary files was not rendered correctly.
* Fixed a bug where the installed python packages were not listed in the log.
* Enable the use of R expressions in the yaml configuration file.
* Enables the user to define the working directory for each script with the `execute_dir` option.

# whirl 0.1.6
* Added support for logging of Python scripts with `run()`.
* Improved unit tests for `run()`.
* Fixing a bug where the queue was not returned correctly in some instances.
* Switched to using `Sys.glob()` instead of `utils::glob2rx()`.

# whirl 0.1.4
* Add `use_whirl()` utility function.

# whirl 0.1.3
* Adding additional arguments to `run()` allowing the user to:
  - control the verbosity level
  - specify whether renv should be checked
  - specify which files to track
  - adjust the output format of the log files.

# whirl 0.1.1
* Fix enabling rendering of md log formats("gfm", "commonmark", "markua").

# whirl 0.1.0
* First version publically available on GitHub.

# whirl 0.0.5
* Updated documentation
* README and vignettes are now ready for users.

# whirl 0.0.4
* Adjusting `run()` to unify execution of scripts, lists of scripts, and configuration files.
* Using multiple independent `callr::r_session` when executing several scripts.
* Cleanup of namespace and exported functions.

# whirl 0.0.3
* Initial version of `run()`.
* Substituting spinner with progress bar when executing single scripts.

# whirl 0.0.2
* Update so that the execution (including order of execution) can be controlled through a config file.

# whirl 0.0.1
* First version of package.
