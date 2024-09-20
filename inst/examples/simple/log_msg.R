
library(whirl)

#' This is a test script for user log messages
#' Read, write, and delete operations can be logged the following way:

log_read("adam.adsl")
log_read("sdtm.vs")

log_delete("adam.adds")

Sys.sleep(4)

log_write("adam.advs")

log_write("output.table.ext")

#' Behind the scenes the log messages are stored in a temporary file to be
#' read into the log report.
#'
#' The file can be accessed via the environment variable "WHIRL_LOG_MSG":

Sys.getenv("WHIRL_LOG_MSG")

Sys.getenv("WHIRL_LOG_MSG") |>
  readLines() |>
  cat(sep = "\n")
