test_that("testing run()", {
  file_config <- system.file("examples", "sequence_exuction", "with_whirl_file", "_whirl.yaml", package = "whirl")
  no_config <- system.file("examples", "simple", package = "whirl")

  withr::with_dir(tempdir(), {
    #Only a config file
    run(path = file_config) |>
      expect_no_error()
  })

  withr::with_dir(tempdir(), {
    #No config file
    #Only a config file
    run(path = no_config) |>
      expect_no_error()
  })

  withr::with_dir(tempdir(), {
    #Both config and folder
    run(path = c(file_config, no_config)) |>
      expect_no_error()
  })

})



