#' Generate a summary tibble from a queue table
#'
#' This function takes a queue table as input and generates a summary tibble
#' with columns for Directory, Filename, Status, Hyperlink, and Information.
#'
#' @param queue_table The queue table containing the result data
#' @return A tibble summarizing the queue table data
#' @noRd
util_queue_summary <- function(queue_table) {
  if (!"result" %in% names(queue_table) ||
        !is.list(queue_table$result)) {
    stop("queue_table must contain a list named 'result'")
  }

  if (!all(sapply(queue_table$result, function(x) {
    all(
      c("log_details", "status") %in% names(x)
    )
  }))) {
    stop("Each result in queue_table must contain 'log_details' and 'status'")
  }

  tibble::tibble(
    Directory = sapply(queue_table[["result"]], function(x) {
      normalizePath(dirname(x$log_details$script), winslash = "/")
    }),
    Filename = sapply(queue_table[["result"]], function(x) {
      basename(x$log_details$script)
    }),
    Status = sapply(queue_table[["result"]], function(x) {
      x$status$status
    }),
    Hyperlink = sapply(queue_table[["result"]], function(x) {
      x$log_details$location
    }),
    Information = sapply(queue_table[["result"]], function(x) {
      paste(x$status$warning, collapse = "<br>")
    })
  ) |>
    tidyr::unnest(cols = c("Information"), keep_empty = TRUE) |>
    tidyr::replace_na(list(Information = ""))
}
