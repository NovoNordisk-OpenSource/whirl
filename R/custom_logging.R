#' Helper function to log custom messages
#'
#' Useful for e.g. read and write operations on databases etc.
#' that are not automatically captured.
#'
#' The default environment variable `WHIRL_LOG_MSG` is set in the session used
#' to log scripts, and input is automatically captured in the resulting log.
#'
#' If run outside of whirl, meaning when the above environment variable is
#' unset, the operations are streamed to `stdout()`. By default the console.
#'
#' @name custom_logging
#' @param file [character()] description of the file that was read, written or
#' deleted.
#' @param log [character()] path to the log file.
#' @examples
#' # Stream logs to console since `WHIRL_LOG_MSG` is not set:
#' log_read("my/folder/input.txt")
#' log_write("my/folder/output.txt")
#' log_delete("my/folder/old_output.txt")
NULL

#' @rdname custom_logging
#' @export
log_read <- function(file, log = Sys.getenv("WHIRL_LOG_MSG")) {
  write_to_log(file, "read", log)
}

#' @rdname custom_logging
#' @export
log_write <- function(file, log = Sys.getenv("WHIRL_LOG_MSG")) {
  write_to_log(file, "write", log)
}

#' @rdname custom_logging
#' @export
log_delete <- function(file, log = Sys.getenv("WHIRL_LOG_MSG")) {
  write_to_log(file, "delete", log)
}

#' @noRd
write_to_log <- function(
    file,
    type = c("read", "write", "delete"),
    log = Sys.getenv("WHIRL_LOG_MSG")) {
  type <- rlang::arg_match(type)
  stopifnot(rlang::is_string(file))
  stopifnot(rlang::is_string(log))

  x <- log_df(
    type = type,
    file = file
  )

  if (log == "") {
    jsonlite::stream_out(x = x, verbose = FALSE)
  } else {
    con <- file(description = log, open = "a")
    jsonlite::stream_out(x = x, con = con, verbose = FALSE)
    close(con)
  }
}

#' @noRd
read_from_log <- function(log = Sys.getenv("WHIRL_LOG_MSG"), track_files) {
  if (log == "" || !file.exists(log)) {
    if (!track_files) {
      return(NULL)
    }
    return(log_df())
  }

  con <- file(description = log, open = "r")
  log_info <- jsonlite::stream_in(con, verbose = FALSE)
  close(con)
  log_info$time <- as.POSIXct(log_info$time)
  return(log_info)
}

#' @noRd
log_df <- function(type = character(), file = character()) {
  data.frame(
    time = rep(x = Sys.time(), times = length(file)),
    type = type,
    file = file
  )
}

#' @noRd
split_log <- function(x, types = c("read", "write", "delete")) {
  if (is.null(x)) {
    return(NULL)
  }

  # Split in a tibble for each type of output
  x <- split(x[c("time", "file")], x$type)

  # Add empty table for types not reported
  out <- vector(mode = "list", length = length(types)) |>
    rlang::set_names(types)

  out[names(x)] <- x

  i <- lapply(X = out, FUN = is.null) |>
    unlist() |>
    which()

  out[i] <- list(log_df()[c("time", "file")])

  lapply(X = out, FUN = `rownames<-`, value = NULL)
}
