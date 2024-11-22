test_that("testing read_glob()", {
  file <- system.file("examples/demo/adam/mk100adsl.R", package = "whirl")
  dir <- system.file("examples/demo/adam", package = "whirl")

  #A config file
  withr::with_dir(tempdir(), {
    got <- read_glob(input = file)
    expect_identical(got, file)
  })

  #Using glob
  withr::with_dir(tempdir(), {
    all_files <- read_glob(input = paste0(dir, "/*.R"))
    expect_true(length(all_files) > 1)
  })

  # A non-existing file
  withr::with_dir(tempdir(), {
    read_glob(input = paste0(dir, "/donotexsist.R")) |>
    expect_message()
  })

})
