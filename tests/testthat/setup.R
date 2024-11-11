## whirl have to be installed for tests
pkgs <- installed.packages() |> row.names()
test <- any(pkgs %in% "whirl")

if (!test) {
  cli::cli_abort("The package whirl have to be installed for subprocesses")
}

# Minimal prints to make it easier to read test output

options(whirl.verbosity_level = "quiet")

# Helper function to select test scripts

test_script <- function(script) {
  script <- system.file("examples/simple", script, package = "whirl")
  stopifnot(all(nchar(script) > 0))
  return(script)
}
