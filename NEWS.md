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
