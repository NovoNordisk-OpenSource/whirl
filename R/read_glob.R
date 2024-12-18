#' @title Fetch files if path uses regular expression
#' @param input A character vector with paths to files that should be executed.
#' @return A character vector with the paths to the files. If regexp have been
#'   used as input these will be solved to the actual files matching the
#'   criteria.
#' @noRd

read_glob <- function(input) {
  files_ <- lapply(input, function(x) {
    # If the file exist then return the path
    if (file.exists(x)) {
      return(x)
    } else {
      # If the file does not exist then check if it is a glob
      files <- Sys.glob(x)
      if (length(files) == 0) {
        cli::cli_alert_warning("No files or folders for this path {x}")
      }
      return(files)
    }
  }) |>
    unlist(use.names = FALSE)

  return(files_)
}
