test_that("testing read_regexp()", {
  file <- system.file("examples/demo/adam/mk100adsl.R", package = "whirl")
  dir <- system.file("examples/demo/adam", package = "whirl")

  #A config file
  withr::with_dir(tempdir(), {
    got <- read_regexp(input = file)
    expect_identical(got, file)
  })

  #Using regexp
  withr::with_dir(tempdir(), {
    all_files <- read_regexp(input = paste0(dir, "/*.R"))
    expect_true(length(all_files) > 1)
  })

  #Using regexp
  withr::with_dir(tempdir(), {
    all_files2 <- read_regexp(input = paste0(dir, "/*.r|R"))
    expect_true(length(all_files2) > 1)
  })


  # A non-existing file
  withr::with_dir(tempdir(), {
    read_regexp(input = paste0(regexp, "/donotexsist.R")) |>
    expect_error()
  })

})
