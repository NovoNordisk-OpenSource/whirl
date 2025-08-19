test_that("python dependencies found correctly", {
  res <- test_script("py_dependencies.py") |>
    run(summary_file = NULL) |>
    expect_no_warning() |>
    expect_no_error()

  info <- res[["result"]][[1]][["session"]]

  info$platform$setting |>
    expect_contains("python")

  info$python$package[info$python$namespaced] |>
    expect_contains(c("pandas", "numpy"))
})
