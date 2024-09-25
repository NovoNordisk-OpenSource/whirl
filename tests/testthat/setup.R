## whirl have to be installed for tests
pkgs <- installed.packages() |> row.names()
test <- any(pkgs %in% "whirl")

if (!test) {
  cli::cli_abort("The package whirl have to be installed for subprocesses")
}

## Minimal cli reporting to have a cleaner test log
options(whirl.verbosity_level = "minimal")
