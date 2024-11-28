# Helper function to select test scripts

test_script <- function(script) {
  script <- test_path("scripts", script) |>
    normalizePath(winslash = "/", mustWork = TRUE)
  return(script)
}
