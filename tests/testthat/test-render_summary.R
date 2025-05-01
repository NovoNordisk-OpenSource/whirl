test_that("warning when summary cant be created", {

  tempdir <- withr::local_tempdir()

  summary_file <- file.path(tempdir, "folder_not_exists", "summary.html")

  q <- whirl_queue$new()$skip("test.R")

  render_summary(input = q, summary_file = summary_file) |>
    expect_error() |>
    expect_warning()
})
