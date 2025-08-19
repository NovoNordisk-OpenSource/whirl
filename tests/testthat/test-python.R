test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()

  res <- test_script("py_dependencies.py") |>
    run(summary_file = NULL) |>
    expect_no_warning() |>
    expect_no_error()

  res[["status"]][[1]] |>
    expect_equal("success")

  info <- res[["result"]][[1]][["session"]]

  info$platform$setting |>
    expect_contains("python")

  info$python$package[info$python$namespaced] |>
    expect_contains(c("pandas", "numpy"))
})
