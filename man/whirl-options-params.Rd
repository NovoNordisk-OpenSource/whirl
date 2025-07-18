% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/whirl-options.R
\name{whirl-options-params}
\alias{whirl-options-params}
\title{Internal parameters for reuse in functions}
\arguments{
\item{verbosity_level}{Verbosity level for functions in whirl.
See \link[zephyr:verbosity_level]{zephyr::verbosity_level} for details.. Default: \code{NA_character_}.}

\item{out_formats}{Which log format(s) to produce. Possibilities are \code{html},
\code{json}, and markdown formats: \code{gfm}, \code{commonmark}, and \code{markua}.. Default: \code{"html"}.}

\item{track_files}{Should files read and written be tracked?
Currently only supported on Linux.. Default: \code{FALSE}.}

\item{check_renv}{Should the projects renv status be checked?. Default: \code{FALSE}.}

\item{track_files_discards}{List of file naming patterns not be tracked when track_files = TRUE. Default: \code{c("^/lib", "^/etc", "^/lib64", "^/usr", "^/var", "^/opt", "^/sys", "^/proc", "^/tmp", "^/null", "^/urandom", "^/.cache")}.}

\item{track_files_keep}{List of file naming patterns always to be tracked when
track_files = TRUE. Default: \code{NULL}.}

\item{approved_packages}{List of approved packages and their version in the format: \{name\}@\{version\}. Default: \code{NULL}.}

\item{n_workers}{Number of simultaneous workers used in the run function.
A maximum of 128 workers is allowed.. Default: \code{1}.}

\item{log_dir}{The output directory of the log files. Default is the folder of
the executed script. log_dir can be a path as a character or it can be a
function that takes the script path as input and returns the log directory.
For more information see the examples of \code{run()} or \code{vignette('whirl')}.. Default: \code{function (x)  dirname(x)}.}

\item{execute_dir}{The working directory of the process executing each script.
Default us to execute R files from the working directory when calling \code{run()}
and all other functions from the directory of the script. To change provide
a character path (used for all scripts) or a function that takes the script
as input and returns the execution directory.. Default: \code{NULL}.}

\item{wait_timeout}{Timeout for waiting for the R process from callr::r_session to
start, in milliseconds.. Default: \code{9000}.}
}
\description{
Internal parameters for reuse in functions
}
\details{
See \link{whirl-options} for more information.
}
\keyword{internal}
