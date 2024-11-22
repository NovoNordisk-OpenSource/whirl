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
    run() |>
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

# test_that("Run yaml config file", {
#
# })
