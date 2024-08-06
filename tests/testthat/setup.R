## whirl have to be installed for tests
pkgs <- installed.packages() |> row.names()
test <- any(pkgs %in% "whirl")

if (!test) {
  cli::cli_abort("The package whirl have to be installed for subprocesses")
}

### Clean up whirl
withr::defer(
  {
    unlink_whirl_error_file()
  },
  teardown_env()
)
