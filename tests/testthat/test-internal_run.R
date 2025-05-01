test_that("testing internal_run()", {
  skip_if_no_quarto()

  # A config file

  q <- whirl_queue$new(n_workers = 2)

  test_script("_whirl.yaml") |>
    internal_run(steps = NULL, level = 1, queue = q) |>
    expect_no_error()

  # A config file calling another config file

  q <- whirl_queue$new()

  test_script("_whirl_to_config.yaml") |>
    internal_run(steps = NULL, level = 1, queue = q) |>
    expect_no_error()
})
