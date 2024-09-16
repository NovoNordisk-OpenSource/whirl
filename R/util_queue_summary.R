#' Generate a summary tibble from a queue table
#'
#' This function takes a queue table as input and generates a summary tibble with columns for Directory, Filename, Status, Hyperlink, and Information.
#'
#' @param queue_table The queue table containing the result data
#' @return A tibble summarizing the queue table data
#' @export
util_queue_summary <- function(queue_table) {
  if (!"result" %in% names(queue_table) || !is.list(queue_table$result)) {
    stop("queue_table must contain a list named 'result'")
  }

  if (!all(sapply(queue_table$result, function(x) all(c("log_details", "status") %in% names(x))))) {
    stop("Each result in queue_table must contain 'log_details' and 'status'")
  }

  tibble::tibble(
    Directory = sapply(queue_table[["result"]], function(x) {
      if ("log_details" %in% names(x) && "script" %in% names(x$log_details)) {
        normalizePath(dirname(x$log_details$script), winslash = "/")
      } else {
        NA
      }
    }),
    Filename = sapply(queue_table[["result"]], function(x) {
      if ("log_details" %in% names(x) && "script" %in% names(x$log_details)) {
        basename(x$log_details$script)
      } else {
        NA
      }
    }),
    Status = sapply(queue_table[["result"]], function(x) {
      if ("status" %in% names(x) && "status" %in% names(x$status)) {
        x$status$status
      } else {
        NA
      }
    }),
    Hyperlink = sapply(queue_table[["result"]], function(x) {
      if ("log_details" %in% names(x) && "location" %in% names(x$log_details)) {
        x$log_details$location
      } else {
        NA
      }
    }),
    Information = sapply(queue_table[["result"]], function(x) {
      if ("status" %in% names(x) && "warning" %in% names(x$status)) {
        x$status$warning
      } else {
        NULL
      }
    })
  ) |>
    tidyr::unnest(cols = c(Information), keep_empty = TRUE) |>
    tidyr::replace_na(list(Information = ""))
}
