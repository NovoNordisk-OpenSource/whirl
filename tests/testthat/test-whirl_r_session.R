test_that("interactive whirl R session components not tested in run", {
  skip_if_no_quarto()
  p <- whirl_r_session$new()

  p$print() |>
    expect_message() |>
    suppressMessages()

  p$get_wd() |>
    dir.exists() |>
    expect_true()

  p$get_wd() |>
    list.files() |>
    sort() |>
    expect_contains(c("dummy.qmd", "log.qmd", "summary.qmd"))

  p$call(func = Sys.sleep, args = list(time = 1)) # Sleep for 1 second

  status <- p$wait(timeout = 10)$check_status() # Timeout after 10 ms
  expect_null(status) # Still running

  status <- p$wait()$check_status()
  expect_equal(status$code, 200L) # Completed successfully

  p$call(func = \() 1 + "a") # Something with an error
  expect_error(p$wait()$check_status())

  # Test temp dir is deleted correctly
  dir <- p$get_wd()
  rm(p)
  gc()

  expect_false(dir.exists(dir))
})

test_that("additional error testing", {
  wrs_report_status(
    status = "unknown",
    script = "my_script.R",
    logs = "my_log.html"
  ) |>
    expect_error()
})
