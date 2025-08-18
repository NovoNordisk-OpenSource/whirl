test_that("progress bar is visible", {
  withr::local_options(
    .new = list(cli.dynamic = TRUE, whirl.verbosity_level = "verbose")
  )

  pb <- pb_start() |>
    expect_no_condition() |>
    expect_type("character")

  queue <- data.frame(
    script = "my_script.R",
    status = "running"
  )

  pb_update(id = pb, queue = queue) |>
    expect_message()

  pb_done(id = pb) |>
    expect_no_error()
})

test_that("progress bar is not visible", {
  pb <- pb_start() |>
    expect_no_condition() |>
    expect_null()

  queue <- data.frame(
    script = "my_script.R",
    status = "running"
  )

  pb_update(id = pb, queue = queue) |>
    expect_no_message()

  pb_done(id = pb) |>
    expect_no_error()
})
