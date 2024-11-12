## whirl have to be installed for tests

devtools::install()

# pkgs <- installed.packages() |> row.names()
# test <- any(pkgs %in% "whirl")
#
# if (!test) {
#   cli::cli_abort("The package whirl have to be installed for subprocesses")
# }

# Minimal prints to make it easier to read test output

withr::local_options(
  list(whirl.verbosity_level = "quiet"),
  .local_envir = teardown_env()
)

# Helper function to select test scripts

test_script <- function(script) {
  script <- test_path("scripts", script)
  stopifnot(file.exists(script))
  return(script)
}
