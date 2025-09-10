# Before calling whirl, add debugging
cat("=== WHIRL DEBUG INFO ===\n")
cat("Working directory:", getwd(), "\n")
cat("Files in current directory:\n")
print(list.files(".", recursive = TRUE, pattern = "\\.py$"))

# Check if the Python script exists and is readable
py_script_path <- "py_dependencies.py"  # or whatever the actual path is
cat("Python script path:", py_script_path, "\n")
cat("Script exists:", file.exists(py_script_path), "\n")
if (file.exists(py_script_path)) {
  cat("Script size:", file.size(py_script_path), "bytes\n")
  cat("Script permissions:", file.access(py_script_path, mode = 4), "\n")  # 4 = read permission
}

# Check what Python executable whirl would use
cat("Python executable from reticulate:", reticulate::py_config()$python, "\n")

# Try to run the Python script manually to see if it works
cat("=== MANUAL PYTHON SCRIPT TEST ===\n")
if (file.exists(py_script_path)) {
  tryCatch({
    result <- system2(reticulate::py_config()$python, py_script_path,
                      stdout = TRUE, stderr = TRUE)
    cat("Manual script execution result:\n")
    cat("STDOUT:\n", paste(result, collapse = "\n"), "\n")
    if (!is.null(attr(result, "status"))) {
      cat("Exit status:", attr(result, "status"), "\n")
    }
  }, error = function(e) {
    cat("Manual script execution failed:", e$message, "\n")
  })
}

cat("=== END WHIRL DEBUG INFO ===\n")



test_that("python dependencies found correctly", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_no_python()

  reticulate::py_require("pandas")
  reticulate::py_require("numpy")

  # DEBUG: Check if packages are actually available
  cat("=== PYTHON ENVIRONMENT DEBUG ===\n")
  cat("Python executable:", reticulate::py_config()$python, "\n")

  # Try different pip commands and show their output
  tryCatch({
    pip_output1 <-  system("python -m pip list", intern = TRUE)
    cat("python -m pip list output:\n")
    cat(paste(pip_output1, collapse = "\n"), "\n")
    cat("---\n")
  }, error = function(e) cat("python -m pip list failed:", e$message, "\n"))

  tryCatch({
    pip_output2 <- system("python3 -m pip list", intern = TRUE)
    cat("python3 -m pip list output:\n")
    cat(paste(pip_output2, collapse = "\n"), "\n")
    cat("---\n")
  }, error = function(e) cat("python3 -m pip list failed:", e$message, "\n"))

  # Test whirl's parse_pip_list function directly
  tryCatch({
    parsed_pip <- system("python -m pip list", intern = TRUE) |> parse_pip_list()
    cat("parse_pip_list() result:\n")
    print(parsed_pip)
    cat("Packages found:", nrow(parsed_pip), "\n")
    if (nrow(parsed_pip) > 0) {
      cat("Package names:", paste(parsed_pip$package, collapse = ", "), "\n")
    }
  }, error = function(e) cat("parse_pip_list() failed:", e$message, "\n"))

  cat("================================\n")

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

  info_py_dependencies <-  res[["result"]][[2]][["session"]]

  info_py_dependencies$platform$setting |>
    expect_contains("python")

  # DEBUG: Show what whirl actually found
  cat("=== WHIRL DETECTION RESULTS ===\n")
  cat("All python packages found by whirl:\n")
  print(info_py_dependencies$python$package)
  cat("Directly used packages:\n")
  print(info_py_dependencies$python$package[info_py_dependencies$python$directly_used])
  cat("directly_used flags:\n")
  print(info_py_dependencies$python$directly_used)
  cat("===============================\n")

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
