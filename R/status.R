#' Get execution status
#'
#' Retrieves errors and warnings from the generated markdown file,
#' and derives the execution status.
#'
#' @noRd

get_status <- function(md) {
  x <- readChar(con = md, nchars = file.size(md)) |>
    paste(collapse = "\n") |>
    stringr::str_split("\n:::")

  x <- x[[1]]

  add_python <- x |>
    stringr::str_detect("\\{.python .cell-code") |>
    any()

  # Errors

  errors <- x |>
    stringr::str_subset(
      pattern = "^ *\\{\\.cell-output \\.cell-output-error\\}"
    ) |>
    stringr::str_remove_all("\\{[^\\}]*\\}") |>
    stringr::str_squish()

  if (add_python) {
    python_errors <- x |>
      stringr::str_subset(pattern = "^ *\\{\\.cell-output") |>
      stringr::str_remove_all("\\{[^\\}]*\\}") |>
      stringr::str_squish() |>
      stringr::str_subset("Error:")

    errors <- c(errors, python_errors)
  }

  # Warnings

  warnings <- x |>
    stringr::str_subset(
      pattern = "^ *\\{\\.cell-output \\.cell-output-stderr\\}"
    ) |>
    stringr::str_remove_all("\\{[^\\}]*\\}") |>
    stringr::str_squish() |>
    stringr::str_subset(pattern = "^(W|w)arning")

  if (add_python) {
    python_warnings <- x |>
      stringr::str_subset(pattern = "^ *\\{\\.cell-output") |>
      stringr::str_remove_all("\\{[^\\}]*\\}") |>
      stringr::str_squish() |>
      stringr::str_subset("Warning:")

    warnings <- c(warnings, python_warnings)
  }

  # Status

  if (length(errors)) {
    status <- "error"
  } else if (length(warnings)) {
    status <- "warning"
  } else {
    status <- "success"
  }

  # Return list with status

  list(
    status = status,
    error = errors,
    warning = warnings
  )
}
