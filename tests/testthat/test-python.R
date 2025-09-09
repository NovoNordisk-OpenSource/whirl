test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()

  reticulate::py_require("pandas")
  reticulate::py_require("numpy")

  res <-  test_script(
    script = c("py_success.py", "py_dependencies.py")
  ) |>
    run(summary_file = NULL) |>
    expect_no_warning() |>
    expect_no_error()

  res[["status"]] |>
    expect_equal(c("success", "success"))

  info_py_success <-  res[["result"]][[1]][["session"]]
  info_py_dependencies <- res[["result"]][[2]][["session"]]

  # DEBUG: Print what we're actually getting
  cat("=== DEBUG INFO ===\n")
  cat("py_success packages:", paste(info_py_success$python$package, collapse = ", "), "\n")
  cat("py_success directly_used:", paste(info_py_success$python$directly_used, collapse = ", "), "\n")
  cat("py_dependencies packages:", paste(info_py_dependencies$python$package, collapse = ", "), "\n")
  cat("py_dependencies directly_used:", paste(info_py_dependencies$python$directly_used, collapse = ", "), "\n")
  cat("==================\n")

  # Your existing tests...
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

