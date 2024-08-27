#' Helper function to log custom messages
#'
#' Useful for e.g. read and write operations on databases etc.
#' that are not automatically captured.
#' @param msg [character()] Message to display in the log
#' @param op [character()] Operation that was performed
#' @export

log_msg <- function(msg, op = c("custom", "read", "write")) {

  op <- rlang::arg_match(op)
  checkmate::assert_string(op)
  checkmate::assert_string(msg)

  x <- data.frame(
    time = Sys.time(),
    op = op,
    msg = msg)

  con <- find_log_msg()
  if (!is.null(con)) {
    con <- file(description = con, open = "a")
    jsonlite::stream_out(x = x, con = con, verbose = FALSE)
    close(con)
  }
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
log_custom <- function(msg) {
  log_msg(msg, "custom")
}

find_log_msg <- function() {
 path <- Sys.getenv("WHIRL_LOG_MSG")
 if (path == "") return(NULL)
 return(path)
}

get_log_msg <- function() {
  con <- find_log_msg()

  if (!is.null(con)) {
    con <- file(description = con, open = "r")
    x <- jsonlite::stream_in(con, verbose = FALSE)
    close(con)
    return(x)
  }
}
