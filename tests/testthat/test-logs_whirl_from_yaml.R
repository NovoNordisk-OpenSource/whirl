# test_that("test from yaml file", {
#   file_ <- system.file("examples", "sequence_exuction", "_whirl.yaml", package = "whirl")
#   file_error <- system.file("examples", "sequence_exuction", "error.yaml", package = "whirl")
#   file_path_and_regexp <- system.file("examples", "sequence_exuction", "path_and_regexp.yaml", package = "whirl")
#
#   withr::with_dir(tempdir(), {
#     run_by_config(file = file_) |>
#       expect_no_error()
#
#     run_by_config(file_error) |>
#       expect_error(regexp = "No files or folders for this path")
#
#     file.copy(
#       from = system.file("examples", package = "whirl"),
#       to = ".", recursive = TRUE, overwrite = TRUE
#     )
#
#     run_by_config(file_path_and_regexp, root_dir = ".") |>
#       expect_no_error()
#   })
# })
