#' Generate a summary tibble from a queue table
#'
#' This function takes a queue table as input and generates a summary tibble
#' with columns for Directory, Filename, Status, Hyperlink, and Information.
#'
#' @param queue_table The queue table containing the result data
#' @return A tibble summarizing the queue table data
#' @noRd
util_queue_summary <- function(queue_table) {
  queue_table |>
    dplyr::mutate(
      Tag = .data$tag,
      Directory = normalizePath(dirname(.data$script), winslash = "/"),
      Filename = basename(.data$script),
      Status = .data$status,
      Hyperlink = vapply(
        X = .data$result,
        FUN = \(x) utils::head(x[["logs"]], 1),
        FUN.VALUE = character(1)
      ),
      Information = vapply(
        X = .data$result,
        FUN = \(x) {
          x[["status"]][c("errors", "warnings")] |>
            unlist() |>
            paste0(collapse = "<br>")
        },
        FUN.VALUE = character(1)
      )
    ) |>
    dplyr::select("Tag", "Directory", "Filename", "Status", "Hyperlink", "Information")
}
