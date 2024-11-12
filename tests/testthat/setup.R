## whirl have to be installed for tests

cat(getwd())
pak::local_install(root = "../../00_pkg_src/whirl", upgrade = FALSE)

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
