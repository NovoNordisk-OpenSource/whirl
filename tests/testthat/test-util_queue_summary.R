test_that("fails with invalid input", {
  data.frame(dummy = "dummy") |>
    util_queue_summary() |>
    expect_error()
})


test_that("Summary tibble is created successfully", {
  skip_if_no_quarto()

  q <- whirl_queue$new(n_workers = 2)

  test_script(c("success.R", "py_success.py")) |>
    q$run()

  q$queue |>
    util_queue_summary() |>
    expect_s3_class("tbl_df") |>
    expect_named(
      c("Tag", "Directory", "Filename", "Status", "Hyperlink", "Information")
    ) |>
    nrow() |>
    expect_equal(2)
})
