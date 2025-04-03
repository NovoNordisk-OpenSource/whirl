test_that("progress bar updates correctly when used", {
  expect_message(
    object = {
      pb <- pb_script$new(script = "test.R", use_progress = TRUE)
    },
    regexp = "test\\.R.*Started"
  )

  pb$update(status = "Running", force = TRUE) |>
    expect_message(regexp = "test\\.R.*Running")

  pb$update(force = TRUE) |>
    expect_message(regexp = "test\\.R.*Running")

  pb$update(status = "new status", force = TRUE) |>
    expect_message(regexp = "test\\.R.*new status")

  pb$done() |>
    expect_message(regexp = "test\\.R.*Completed")

  pb$update() |>
    expect_error()

  pb$done() |>
    expect_error()
})

test_that("if not, only final status is shown", {
  expect_no_message(
    object = {
      pb <- pb_script$new(script = "test.R", use_progress = FALSE)
    }
  )

  pb$update(status = "Running", force = TRUE) |>
    expect_no_message()

  pb$done() |>
    expect_message(regexp = "test\\.R.*Completed")
})

test_that("correct messages are shown depending on execution status", {
  pb_script$new(script = "test.R")$done() |>
    expect_message(regexp = "Completed succesfully")

  pb_script$new(script = "test.R")$done("warning") |>
    expect_message(regexp = "Completed with warnings")

  pb_script$new(script = "test.R")$done("error") |>
    expect_message(regexp = "Completed with errors")
})
