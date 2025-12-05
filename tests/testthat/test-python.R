test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()
  skip_if_not_installed(pkg = "reticulate", minimum_version = "1.41.0")

  reticulate::py_require("numpy")
  reticulate::py_require("pandas")
  reticulate::py_require("pip")

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

  info_py_dependencies <- res[["result"]][[2]][["session"]]

  info_py_dependencies$platform$setting |>
    expect_contains("python")

  info_py_dependencies$python$package[
    info_py_dependencies$python$directly_used
  ] |>
    expect_length(2) |>
    expect_contains(c("pandas", "numpy"))
})

test_that("reticulate::py_list_packages() is consistent", {
  skip_on_cran()
  skip_if_no_python()

  reticulate::py_list_packages() |>
    expect_s3_class("data.frame") |>
    expect_named(c("package", "version", "requirement")) |>
    nrow() |>
    expect_gt(0)
})
