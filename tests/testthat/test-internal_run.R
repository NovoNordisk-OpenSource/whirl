test_that("testing internal_run()", {
  file_config <- system.file("examples/demo/config_to_config.yaml", package = "whirl")

  q <- whirl_queue$new()

  #A config file
  withr::with_dir(tempdir(), {
    internal_run(input = file_config, steps = NULL, level = 1, queue = q) |>
      expect_no_error()
  })


})
