# Test for existence of 'result' in the queue_table
test_that("queue_table must contain a list named 'result'", {
  queue_table <- list(dummy = "dummy")
  expect_error(util_queue_summary(queue_table), "queue_table must contain a list named 'result'")
})

# Test for existence of 'log_details' and 'status' in each result
test_that("Each result in queue_table must contain 'log_details' and 'status'", {
  queue_table <- list(result = list(list(log_details = "dummy")))
  expect_error(util_queue_summary(queue_table), "Each result in queue_table must contain 'log_details' and 'status'")
})

# Test for successful creation of summary tibble
test_that("Summary tibble is created successfully", {
  q <- whirl_queue$new(n_workers = 3)

  c(
  system.file("examples/simple/prg1.R", package = "whirl"),
  system.file("examples/simple/success.R", package = "whirl")
  ) |>
    q$run()

  queue_table <- q$queue
  summary_tibble <- util_queue_summary(queue_table)
  expect_type(summary_tibble, "list")
  expect_equal(ncol(summary_tibble), 5)
  expect_equal(nrow(summary_tibble), 2)
})
