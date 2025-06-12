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
    dplyr::transmute(
      Directory = normalizePath(dirname(script), winslash = "/"),
      Filename = basename(script),
      Status = status,
      Hyperlink = vapply(
        X = result, 
        FUN = \(x) head(x[["logs"]],1),
        FUN.VALUE = character(1)
      ),
      Information = vapply(
        X = result, 
        FUN = \(x) x[["status"]][c("errors", "warnings")] |> 
          unlist() |> 
          paste0(collapse = "<br>"),
        FUN.VALUE = character(1)
      )
    )
}
