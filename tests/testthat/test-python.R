test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()

  # Add diagnostic information
  cat("Python executable:", reticulate::py_config()$python, "\n")
  cat("Python packages location:", reticulate::py_config()$libpaths, "\n")

  reticulate::py_require("pandas")
  reticulate::py_require("numpy")

  res <- test_script(
    script = c("py_success.py", "py_dependencies.py")
  ) |>
    run(summary_file = NULL) |>
    expect_no_warning() |>
    expect_no_error()

  res[["status"]] |>
    expect_equal(c("success", "success"))

  info_py_success <- res[["result"]][[1]][["session"]]

  info_py_success$platform$setting |>
    expect_contains("python")

  info_py_success$python$package[
    info_py_success$python$directly_used
  ] |>
    expect_length(0)

  #skip_on_os("windows") # To be fixed in #204

  info_py_dependencies <- res[["result"]][[2]][["session"]]

  info_py_dependencies$platform$setting |>
    expect_contains("python")

  info_py_dependencies$python$package[
    info_py_dependencies$python$directly_used
  ] |>
    expect_length(2) |>
    expect_contains(c("pandas", "numpy"))
})

test_that("parse_pip_list() is consistent", {
  skip_on_cran()
  skip_if_no_python()

  system("python3 -m pip list -v", intern = TRUE) |>
    parse_pip_list() |>
    expect_s3_class("data.frame") |>
    expect_named(c("package", "version", "path", "installer")) |>
    nrow() |>
    expect_gt(0)
})
