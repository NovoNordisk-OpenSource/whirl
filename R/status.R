#' Get execution status
#'
#' Retrieves errors and warnings from the generated markdown file, and derives the execution status.
#'
#' @noRd

get_status <- function(md) {

  x <- readChar(con = md, nchars = file.size(md)) |>
    paste(collapse = "\n") |>
    stringr::str_split("\n:::")

  x <- x[[1]]

  # Errors

  errors <- x |>
    stringr::str_subset(pattern = "cell-output-error") |>
    stringr::str_remove_all("\\{[^\\}]*\\}") |>
    stringr::str_squish()

  # Warnings

  warnings <- x |>
    stringr::str_subset(pattern = "cell-output-stderr") |>
    stringr::str_remove_all("\\{[^\\}]*\\}") |>
    stringr::str_squish()

  # Status

  if (length(errors)) {
    status = "Error"
  } else if (length(warnings)) {
    status = "Warnings"
  } else {
    status = "Completed"
  }

  # Return list with status

  list(
    status = status,
    errors = errors,
    warnings = warnings
  )
}


