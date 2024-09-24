#' @title Fetch files if path uses regular expression
#' @param input A character vector with paths to files that should be executed.
#' @return A character vector with the paths to the files. If regexp have been
#'   used as input these will be solved to the actual files matching the
#'   criteria.
read_regexp <- function(input) {

  files_ <- lapply(input, function(x) {

    #If the file exist then return the path
    if (fs::file_exists(x)) {
      return(x)
    } else {
      #If the file does not exist then check if it is a regexp
      regexp <- basename(x) |>
        utils::glob2rx()

      files <- list.files(path = dirname(x), pattern = regexp, full.names = TRUE)
      if (length(files) == 0) {
        cli::cli_abort("No files or folders for this path {x}")
      }
      return(files)
    }
  }) |>
    unlist(use.names = FALSE)

  return(files_)
}


