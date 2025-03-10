# whirl dev
* Fix UT for azure CI
* Added pre-commit for developers
* Fixed linting errors
* Fix typo in `track_files_discards` option

# whirl 0.1.7 (2024-12-17)
* Enable redirection of logs through the `log_dir` argument in `run()`.
* Changed the title on the individual logs to the script name and moved the path to a distinct section within the title-block.
* Fixed a bug where the hyperlink in the summary files was not rendered correctly.
* Fixed a bug where the installed python packages were not listed in the log.
* Enable the use of R expressions in the yaml configuration file.
* Enables the user to define the working directory for each script with the `execute_dir` option.

# whirl 0.1.6 (2024-11-25)
* Added support for logging of Python scripts with `run()`.
* Improved unit tests for `run()`
* Fixing a bug where the queue was not returned correctly in some instances.
* Switched to using `Sys.glob()` instead of `utils::glob2rx()`.

# whirl 0.1.5 (2024-11-21)
* Reducing the number of dependencies.

# whirl 0.1.4 (2024-11-01)
* Add `use_whirl()` utility function.

# whirl 0.1.3 (2024-10-23)
* Adding additional arguments to `run()` allowing the user to:
  - control the verbosity level
  - specify whether renv should be checked
  - specify which files to track
  - adjust the output format of the log files.

# whirl 0.1.2 (2024-10-21)
* Updated package website url and example code

# whirl 0.1.1 (2024-10-07)  
* Fix enabling rendering of md log formats("gfm", "commonmark", "markua").

# whirl 0.1.0 (2024-10-01)
* First public release.

# whirl 0.0.5 (2024-09-27)
* Updated documentation
* README and vignettes are now ready for users.

# whirl 0.0.4 (2024-09-24)
* Adjusting `run()` to unify execution of scripts, lists of scripts, and configuration files.
* Using multiple independent `callr::r_session` when executing several scripts.
* Cleanup of namespace and exported functions.

# whirl 0.0.3 (2024-08-26)
* Initial commit of `run()`.
* Substituting spinner with progress bar when executing single scripts.

# whirl 0.0.2
* Update so that the execution (including order of execution) can be controlled through a config file

# whirl 0.0.1
* First release to internal package repository.
