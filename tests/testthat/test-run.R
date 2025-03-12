expect_single_script <- function(res) {
  res[["status"]] |>
    testthat::expect_equal("success")

  res[["result"]][[1]] |>
    names() |>
    testthat::expect_equal(c("status", "session_info_rlist", "log_details"))

  return(invisible(res))
}

test_that("Run single R script", {
  skip_if_no_quarto()
  res <- test_script("success.R") |>
    run() |>
    expect_no_warning() |>
    expect_no_error()

  expect_single_script(res)
})

test_that("Run single python script", {
  skip_if_no_quarto()
  res <- test_script("py_success.py") |>
    run() |>
    expect_no_warning() |>
    expect_no_error()

  expect_single_script(res)
})

expect_multiple_scripts <- function(res) {
  res[["status"]] |>
    testthat::expect_equal(c("success", "warning", "error"))

  res[["result"]][[1]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    testthat::expect_equal(c(FALSE, FALSE), ignore_attr = TRUE)

  res[["result"]][[2]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    testthat::expect_equal(c(FALSE, TRUE), ignore_attr = TRUE)

  res[["result"]][[3]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    testthat::expect_equal(c(TRUE, FALSE), ignore_attr = TRUE)

  return(invisible(res))
}

test_that("Run multiple R scripts", {
  skip_if_no_quarto()
  res <- test_script(c("success.R", "warning.R", "error.R")) |>
    run(n_workers = 2) |>
    expect_no_error()

  expect_multiple_scripts(res)
})

test_that("Run multiple python scripts", {
  skip_if_no_quarto()
  res <- test_script(c("py_success.py", "py_warning.py", "py_error.py")) |>
    run(n_workers = 2) |>
    expect_no_error()

  expect_multiple_scripts(res)
})

test_that("Run yaml config file", {
  skip_if_no_quarto()
  res <- test_script("_whirl.yaml") |>
    run(n_workers = 2) |>
    expect_no_error()
})

test_that("Change the log_dir to a path", {
  skip_if_no_quarto()
  # Custom path
  custom_path <- withr::local_tempdir()

  # Execute run() with log_dir = custom path
  res <- test_script("success.R") |>
    run(log_dir = custom_path) |>
    expect_no_error()

  # Check if the log file is created in the custom path
  file.path(custom_path, "success_log.html") |>
    file.exists() |>
    expect_true()
})

test_that("Change the log_dir with a function", {
  skip_if_no_quarto()
  # Custom path and copy script
  custom_path <- withr::local_tempdir()
  dir.create(file.path(custom_path, "logs"))
  file.copy(from = test_script("warning.R"), to = custom_path) |>
    expect_true()

  # Execute run() with log_dir as a function
  res <- file.path(custom_path, "warning.R") |>
    run(log_dir = function(x) {
      paste0(dirname(x), "/logs")
    }) |>
    expect_no_error()

  # Check if the log file is created in the correct folder
  file.path(custom_path, "logs", "warning_log.html") |>
    file.exists() |>
    expect_true()
})

test_that("Change the execute_dir to a path", {
  skip_if_no_quarto()
  custom_path <- withr::local_tempdir()
  withr::local_options(whirl.execute_dir = custom_path)

  test_script("success.R") |>
    run() |>
    expect_no_error()

  withr::local_options(whirl.execute_dir = "this/path/does/not/exist")

  test_script("success.R") |>
    run() |>
    expect_error()
})

test_that("Change the execute_dir to a function", {
  skip_if_no_quarto()
  withr::local_options(whirl.execute_dir = \(x) dirname(x))

  test_script("success.R") |>
    run() |>
    expect_no_error()
})
