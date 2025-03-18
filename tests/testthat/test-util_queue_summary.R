# Test for existence of 'result' in the queue_table
test_that("queue_table must contain a list named 'result'", {
  list(dummy = "dummy") |>
    util_queue_summary() |>
    expect_error("queue_table must contain a list named 'result'")
})

# Test for existence of 'log_details' and 'status' in each result
test_that(
  "Each result in queue_table must contain 'log_details' and 'status'",
  {
    list(result = list(list(log_details = "dummy"))) |>
      util_queue_summary() |>
      expect_error(
        "Each result in queue_table must contain 'log_details' and 'status'"
      )
  }
)

# Test for successful creation of summary tibble

test_that("Summary tibble is created successfully", {
  skip_if_no_quarto()
  q <- whirl_queue$new(n_workers = 2)

  test_script(c("success.R", "py_success.py")) |>
    q$run()

  q$queue |>
    util_queue_summary() |>
    expect_s3_class("tbl_df") |>
    expect_named(
      c("Directory", "Filename", "Status", "Hyperlink", "Information")
    ) |>
    nrow() |>
    expect_equal(2)
})
