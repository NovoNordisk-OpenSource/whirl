# test_that("Execute multiple Scripts", {
#   scripts_list <- c("prg1.R")
#
#   withr::with_tempdir({
#     # Setup: Copy example scripts to temp directory
#     file.copy(
#       from = system.file("examples", "simple", scripts_list, package = "whirl"),
#       to = getwd(), overwrite = TRUE
#     )
#
#     # Setup: Copy example folder to temp directory
#
#     dir.create("simple")
#     file.copy(
#       from = system.file("examples", "simple", package = "whirl"),
#       to = "./simple", recursive = TRUE
#     )
#     scripts_folder <- file.path(".", "simple")
#
#     # Test for valid input with folder path
#     expect_s3_class(
#       run_paths(paths = scripts_folder),
#       "data.frame"
#     )
#
#     # Test for valid input with script paths
#     expect_s3_class(
#       run_paths(paths = scripts_list),
#       "data.frame"
#     )
#
#     # Test for invalid input
#     expect_error(
#       run_paths(paths = 123)
#     )
#
#     # Test parallel execution
#     expect_s3_class(
#       run_paths(
#         paths = scripts_list,
#         parallel = TRUE,
#         num_cores = 2
#       ),
#       "data.frame"
#     )
#
#     # Test sequential execution
#     expect_s3_class(
#       run_paths(
#         paths = scripts_list,
#         parallel = FALSE
#       ),
#       "data.frame"
#     )
#   })
# })
