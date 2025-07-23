test_that("fails with invalid input", {
  data.frame(dummy = "dummy") |>
    util_queue_summary() |>
    expect_error()
})

test_that("Summary tibble is created successfully", {
  skip_if_no_quarto()

  # Check Python availability and adjust scripts accordingly
  python_available <-  tryCatch({
    reticulate::py_available(initialize = TRUE)
  }, error = function(e) FALSE)

  q <- whirl_queue$new(n_workers = 1, verbosity_level = "quiet")

  # Use appropriate scripts based on Python availability
  if (python_available) {
    scripts <- test_script(c("success.R", "py_success.py"))
    expected_rows <- 2
  } else {
    scripts <- test_script("success.R")
    expected_rows <- 1
  }

  scripts |> q$run()

  q$queue |>
    util_queue_summary() |>
    expect_s3_class("tbl_df") |>
    expect_named(
      c("Tag", "Directory", "Filename", "Status", "Hyperlink", "Information")
    ) |>
    nrow() |>
    expect_equal(expected_rows)
})
