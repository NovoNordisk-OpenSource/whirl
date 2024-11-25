## whirl have to be installed for Quarto to use it

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env()
)

# Minimal prints to make it easier to read test output

withr::local_options(
  list(whirl.verbosity_level = "quiet"),
  .local_envir = teardown_env()
)

# Helper function to select test scripts

test_script <- function(script) {
  script <- test_path("scripts", script) |>
    normalizePath(winslash = "/", mustWork = TRUE)
  return(script)
}
