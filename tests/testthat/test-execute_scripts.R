library(testthat)
library(withr)

test_that("Execute multiple Scripts", {
  scripts_list <- c("prg1.R")

  withr::with_tempdir({
    # Setup: Copy example scripts to temp directory
    file.copy(from = system.file("examples", scripts_list, package = "whirl"),
              to = getwd(), overwrite = TRUE)

    # Setup: Copy example folder to temp directory
    dest_folder <- file.path(tempdir())

    file.copy(from = system.file("examples", package = "whirl"),
              to = dest_folder, recursive = TRUE)
    scripts_folder <- file.path(tempdir(), "examples")

    # Test for valid input with folder path
    expect_s3_class(
      execute_scripts(scripts = NULL, folder = scripts_folder),
      "data.frame"
    )

    # Test for valid input with script paths
    expect_s3_class(
      execute_scripts(scripts = scripts_list, folder = NULL),
      "data.frame"
    )

    # Test for invalid input
    expect_error(
      execute_scripts(scripts = 123, folder = NULL)
    )

    # Test parallel execution
    expect_s3_class(
      execute_scripts(
        scripts = scripts_list,
        folder = NULL,
        parallel = TRUE,
        num_cores = 2
      ),
      "data.frame"
    )

    # Test sequential execution
    expect_s3_class(
      execute_scripts(
        scripts = scripts_list,
        folder = NULL,
        parallel = FALSE
      ),
      "data.frame"
    )
  })
})
