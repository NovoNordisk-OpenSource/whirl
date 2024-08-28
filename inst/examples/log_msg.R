
#' This is a test script for user log messages

whirl::log_msg("adam.adsl", "read")
whirl::log_msg("sdtm.vs", "read")

whirl::log_read("adam.adds")

Sys.sleep(4)

whirl::log_msg("adam.advs", "write")

whirl::log_write("output.table.ext")

#' Is this interface nice for the user or would it better with:

# whirl::log_read("adam.adsl")
# whirl::log_write("adam.advs")

#' And no support for custom messages?
#'
#' Make them available as standalone functions to plug into a package
#' without having to import whirl.
#'
#' See:
#'
#' - [usethis::use_standalone()](https://usethis.r-lib.org/reference/use_standalone.html)
#' - [standalone-rlang](https://github.com/r-lib/rlang/blob/main/R/standalone-rlang.R)
#'
#' Show how it looks behind the scenes:

Sys.getenv("WHIRL_LOG_MSG")

Sys.getenv("WHIRL_LOG_MSG") |>
  readLines() |>
  cat(sep = "\n")
