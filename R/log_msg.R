#' Helper function to log custom messages
#'
#' Useful for e.g. read and write operations on databases etc.
#' that are not automatically captured.
#' @param msg [character()] Message to display in the log
#' @param type [character()] Operation that was performed
#' @export

log_msg <- function(msg, type = c("read", "write", "delete")) {

  con <- find_log_msg()
  if (is.null(con)) {
    return()
  }

  type <- rlang::arg_match(type)
  checkmate::assert_string(type)
  checkmate::assert_string(msg)

  x <- data.frame(
    time = Sys.time(),
    type = type,
    file = file
    )

  con <- file(description = con, open = "a")
  jsonlite::stream_out(x = x, con = con, verbose = FALSE)
  close(con)
}

#' @rdname log_msg
#' @export
log_read <- function(msg) {
  log_msg(msg, "read")
}

#' @rdname log_msg
#' @export
log_write <- function(msg) {
  log_msg(msg, "write")
}

#' @rdname log_msg
#' @export
log_delete <- function(msg) {
  log_msg(msg, "delete")
}

find_log_msg <- function() {
 path <- Sys.getenv("WHIRL_LOG_MSG")
 if (path == "") return(NULL)
 return(path)
}

read_log_info <- function(types = c("read", "write", "delete")) {
  con <- find_log_msg()

  if (is.null(con)) {
    return(NULL)
  }

  con <- file(description = con, open = "r")
  log_info <- jsonlite::stream_in(con, verbose = FALSE)
  close(con)

  class(log_info) <- c("whirl_log_info", class(log_info))

  # Split in a tibble for each type of output

  log_info <- split(log_info[c("time", "file")], log_info$type)

  # Add empty table for types not reported

  out <- vector(mode = "list", length = length(types)) |>
    rlang::set_names(types)

  out[names(log_info)] <- log_info

  i <- lapply(X = out, FUN = is.null) |>
    unlist() |>
    which()
  dummy <- tibble::tibble(file = "No files")
  class(dummy) <- c("whirl_log_info", class(dummy))
  out[i] <- list(dummy)

  return(out)
}

#' @noRd

knit_print.whirl_log_info <- function(x, ...) {
  x |>
    knitr::kable() |>
    knitr::knit_print()
}
