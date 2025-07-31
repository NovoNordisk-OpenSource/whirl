test_that("whirl_queue edge cases not covered in test-run etc.", {
  q <- whirl_queue$new()

  q$skip("skipped.R")

  expect_equal(q$queue$script, "skipped.R")
  expect_equal(q$queue$status, "skipped")

  q |>
    print() |>
    expect_snapshot()

  q$push("a/b/new.R") |>
    expect_error("Logs cannot be saved because \"a/b\" does not exist")

  q$push(c("a/new.R", "b/also_new.R")) |>
    expect_error("Logs cannot be saved because \"a\" and \"b\" does not exist")

  whirl_queue$new(log_dir = "fake_folder")$push("fake_script.R") |>
    expect_error("Logs cannot be saved because \"fake_folder\" does not exist")
})
