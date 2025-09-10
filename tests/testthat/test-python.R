test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()

  # Add diagnostic information
  cat("Python executable:", reticulate::py_config()$python, "\n")
  cat("Python packages location:", reticulate::py_config()$libpaths, "\n")

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

  info_py_success$platform$setting |>
    expect_contains("python")

  info_py_success$python$package[
    info_py_success$python$directly_used
  ] |>
    expect_length(0)

  #skip_on_os("windows") # To be fixed in #204

  info_py_dependencies <-  res[["result"]][[2]][["session"]]

  info_py_dependencies$platform$setting |>
    expect_contains("python")

  # Add detailed diagnostics for the failing test
  cat("\n=== DEBUGGING INFO FOR py_dependencies.py ===\n")
  cat("All Python packages found:\n")
  if (!is.null(info_py_dependencies$python$package)) {
    print(info_py_dependencies$python$package)
  } else {
    cat("No packages found!\n")
  }

  cat("Directly used flags:\n")
  if (!is.null(info_py_dependencies$python$directly_used)) {
    print(info_py_dependencies$python$directly_used)
  } else {
    cat("No directly_used flags found!\n")
  }

  cat("Packages marked as directly used:\n")
  directly_used_packages <-  info_py_dependencies$python$package[info_py_dependencies$python$directly_used]
  if (length(directly_used_packages) > 0) {
    print(directly_used_packages)
  } else {
    cat("No packages marked as directly used!\n")
  }

  cat("Expected packages: pandas, numpy\n")
  cat("Length of directly used packages:", length(directly_used_packages), "\n")
  cat("=== END DEBUGGING INFO ===\n\n")

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
