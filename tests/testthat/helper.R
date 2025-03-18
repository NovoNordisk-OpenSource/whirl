# Helper function to select test scripts

test_script <- function(script) {
  script <- testthat::test_path("scripts", script) |>
    normalizePath(winslash = "/", mustWork = TRUE)
  return(script)
}

# Use to test quarto availability or version lower than - from the quarto package
skip_if_no_quarto <- function(ver = NULL) {
  skip_if(is.null(quarto::quarto_path()), message = "Quarto is not available")
  skip_if(
    quarto::quarto_version() < ver,
    message = sprintf("Version of quarto is lower than %s: %s.", ver,  quarto::quarto_version())
  )
}
