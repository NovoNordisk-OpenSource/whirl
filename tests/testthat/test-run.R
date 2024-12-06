test_that("Run single R script", {

  res <- test_script("success.R") |>
    run() |>
    expect_no_condition()

  res[["status"]] |>
    expect_equal("success")

  res[["result"]][[1]] |>
    names() |>
    expect_equal(c("status", "session_info_rlist", "log_details"))

})

test_that("Run single python script", {

  res <- test_script("py_success.py") |>
    run() |>
    expect_no_condition()

  res[["status"]] |>
    expect_equal("success")

  res[["result"]][[1]] |>
    names() |>
    expect_equal(c("status", "session_info_rlist", "log_details"))

})

test_that("Run multiple R scripts", {

  res <- test_script(c("success.R", "warning.R", "error.R")) |>
    run(n_workers = 2) |>
    expect_no_error()

  res[["status"]] |>
    expect_equal(c("success", "warning", "error"))

  res[["result"]][[1]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(FALSE, FALSE), ignore_attr = TRUE)

  res[["result"]][[2]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(FALSE, TRUE), ignore_attr = TRUE)

  res[["result"]][[3]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(TRUE, FALSE), ignore_attr = TRUE)
})

test_that("Run multiple python scripts", {

  res <- test_script(c("py_success.py", "py_warning.py", "py_error.py")) |>
    run(n_workers = 2) |>
    expect_no_error()

  res[["status"]] |>
    expect_equal(c("success", "warning", "error"))

  res[["result"]][[1]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(FALSE, FALSE), ignore_attr = TRUE)

  res[["result"]][[2]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(FALSE, TRUE), ignore_attr = TRUE)

  res[["result"]][[3]][["status"]][c("error", "warning")] |>
    lapply(\(x) length(x) > 0) |>
    unlist() |>
    expect_equal(c(TRUE, FALSE), ignore_attr = TRUE)
})

test_that("Run yaml config file", {

  res <- test_script("_whirl.yaml") |>
    run(n_workers = 2) |>
    expect_no_error()

})


test_that("Change the log_dir to a path", {
  #Custom path
  custom_path <- test_path("scripts/logs") |>
    normalize_with_base(base = getwd())

  #Create the dir if it does not exist
  if (!file.exists(custom_path)) {
    dir.create(custom_path)
  } else {
    #If it exists then clean the folder
    list.files(custom_path, full.names = TRUE) |>
      file.remove()
  }

  #Execute run() with log_dir = custom path
  res <- test_script("success.R") |>
    run(log_dir = custom_path) |>
    expect_no_error()

  #Check if the log file is created in the custom path
  expect_true(file.exists(test_script("logs/success_log.html")))

  #Clean the folder
  list.files(custom_path, full.names = TRUE) |>
    file.remove()

})


test_that("Change the log_dir with a function", {
  #Custom path
  custom_path <- test_path("scripts/logs") |>
    normalize_with_base(base = getwd())

  #Create the dir if it does not exist
  if (!file.exists(custom_path)) {
    dir.create(custom_path)
  } else {
  #If it exists then clean the folder
  list.files(custom_path, full.names = TRUE) |>
    file.remove()
  }

  #Execute run() with log_dir as a function
  res <- test_script("warning.R") |>
    run(log_dir = function(x) {paste0(dirname(x), "/logs")}) |>
    expect_no_error()

  #Check if the log file is created in the correct folder
  expect_true(file.exists(test_script("logs/warning_log.html")))

  #Clean the folder
  list.files(custom_path, full.names = TRUE) |>
    file.remove()
})

