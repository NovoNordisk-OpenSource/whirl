# Helper function to select test scripts

test_script <- function(script) {
  script <- testthat::test_path("scripts", script) |>
    normalizePath(winslash = "/", mustWork = TRUE)
  return(script)
}

# Use to test quarto availability or version lower than required
skip_if_no_quarto <- function(ver = NULL) {
  skip_if(is.null(quarto::quarto_path()), message = "Quarto is not available")
  skip_if(
    condition = quarto::quarto_version() < ver,
    message = sprintf(
      fmt = "Version of quarto is lower than %s: %s.",
      ver,
      quarto::quarto_version()
    )
  )
}

# Use to test if python is available for simple tests
skip_if_no_python <- function() {
  skip_if(
    condition = !reticulate::py_available(initialize = TRUE),
    message = "Python is not available"
  )
}
