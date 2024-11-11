# test_that("testing internal_run()", {
#   file_config <- system.file("examples/demo/tfl/tfl_whirl.yaml", package = "whirl")
#   config_to_config <- system.file("examples/demo/config_to_config.yaml", package = "whirl")
#
#   # A config file
#
#   queue <- whirl_queue$new()
#   internal_run(input = file_config, steps = NULL, level = 1, queue = queue) |>
#     expect_no_error()
#
#   # A config file calling another config file
#
#   queue <- whirl_queue$new()
#   internal_run(input = config_to_config, steps = NULL, level = 1, queue = queue) |>
#     expect_no_error()
#
# })
