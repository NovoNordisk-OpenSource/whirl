#' @title Fetch files if path uses regular expression
#' @param input A character vector with paths to files that should be executed.
#' @return A character vector with the paths to the files. If regexp have been
#'   used as input these will be solved to the actual files matching the
#'   criteria.
#' @noRd

read_glob <- function(input, root_dir) {
  files <- vector(
    mode = "list",
    length = length(input)
  )

  for (i in seq_along(files)) {
    if (length(input[[i]]) > 1 || is.list(input[[i]])) {
      files[[i]] <- read_glob(
        input = input[[i]],
        root_dir = root_dir
      )
    } else {
      files[[i]] <- input[[i]] |>
        normalize_with_base(base = root_dir) |>
        read_single_glob()
    }
  }

  if (is.list(input)) {
    return(files)
  }

  unlist(files, use.names = FALSE)
}

#' @noRd
read_single_glob <- function(x) {
  if (file.exists(x)) {
    return(x)
  }
  files <- Sys.glob(x)
  if (length(files) == 0) {
    cli::cli_alert_warning("No files or folders for this path {x}")
  }
  files
}
