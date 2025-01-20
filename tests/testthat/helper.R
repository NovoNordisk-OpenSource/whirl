# Helper function to select test scripts

test_script <- function(script) {
  script <- testthat::test_path("scripts", script) |>
    normalizePath(winslash = "/", mustWork = TRUE)
  return(script)
}
