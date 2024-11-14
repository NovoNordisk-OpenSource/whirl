## whirl have to be installed for Quarto to use it

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env()
)
if (interactive()) { # devtools::test
  pkg_path <- "../.."
} else { # R CMD Check
  pkg_path <- "../../00_pkg_src/whirl"
}
pak::local_install(
  root = pkg_path,
  upgrade = FALSE,
  dependencies = FALSE,
  ask = FALSE
)

# Minimal prints to make it easier to read test output

withr::local_options(
  list(whirl.verbosity_level = "quiet"),
  .local_envir = teardown_env()
)

# Helper function to select test scripts

test_script <- function(script) {
  script <- test_path("scripts", script) |>
    normalizePath()
  stopifnot(file.exists(script))
  return(script)
}
