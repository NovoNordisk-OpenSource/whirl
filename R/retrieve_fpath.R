#' Retrieve path of file from folder/subfolders
#'
#' @param in_file - file to search path for
#'
#' @return A path to a file
#' @export
#'
#' @examples
#' retrieve_fpath("prg1.R")
retrieve_fpath <- function(in_file) {
  tryCatch(
    expr = {
      list.files(
        pattern = in_file,
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE
      )[[1]] |> normalizePath(winslash = "/")
    },
    error = function(e){
      stop(paste("File does not seem to exist: ", in_file))
    }
  )
}
