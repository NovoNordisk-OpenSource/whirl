#' Helper function to log custom messages
#'
#' Useful for e.g. read and write operations on databases etc.
#' that are not automatically captured.
#' @name custom_logging
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
  checkmate::assert_string(type)
  checkmate::assert_string(file)
  checkmate::assert_string(log)
  time <- Sys.time()

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
read_from_log <- function(log = Sys.getenv("WHIRL_LOG_MSG")) {

  if (log == "" || !file.exists(log)) {
    return(log_df())
  }

  con <- file(description = log, open = "r")
  log_info <- jsonlite::stream_in(con, verbose = FALSE) |>
    dplyr::mutate(
      time = as.POSIXct(time)
    )
  close(con)
  return(log_info)
}

#' @noRd
log_df <- function(type = character(), file = character()) {
  data.frame(
    time = Sys.time()[length(file)],
    type = type,
    file = file
  )
}

#' @noRd
split_log <- function(log_df, types = c("read", "write", "delete")) {

  class(log_df) <- c("whirl_log_info", class(log_df))

  # Split in a tibble for each type of output

  log_df <- split(log_df[c("time", "file")], log_df$type)

  # Add empty table for types not reported

  out <- vector(mode = "list", length = length(types)) |>
    rlang::set_names(types)

  out[names(log_df)] <- log_df

  i <- lapply(X = out, FUN = is.null) |>
    unlist() |>
    which()
  dummy <- data.frame(file = "No files")
  class(dummy) <- c("whirl_log_info", class(dummy))
  out[i] <- list(dummy)

  return(out)
}

#' @noRd
knit_print.whirl_log_info <- function(x, ...) {
  x |>
    knitr::kable(
      row.names = FALSE
    ) |>
    knitr::knit_print()
}
