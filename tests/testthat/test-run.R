test_that("testing run()", {
  file_config <- system.file("examples/demo/metadata/metadata_whirl.yaml", package = "whirl")
  directory <- system.file("examples/simple", package = "whirl")
  file <- system.file("examples/demo/adam/mk100adsl.R", package = "whirl")

  #A config file
  withr::with_dir(tempdir(), {
    run(input = file_config) |>
      expect_no_error()
  })

  #Pointing to a directory
  withr::with_dir(tempdir(), {
    run(input = directory) |>
      expect_error()
  })

  # A file
  withr::with_dir(tempdir(), {
    run(input = file, n_workers = 1) |>
      expect_no_error()
  })

  # A list
  withr::with_dir(tempdir(), {
    run(input = list(file)) |>
      expect_no_error()
  })

})



