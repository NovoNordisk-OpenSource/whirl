test_that("testing enrich_input()", {
  file <- system.file("examples/demo/adam/mk100adsl.R", package = "whirl")
  regexp <- system.file("examples/demo/adam/", package = "whirl")



  #A config file
  withr::with_dir(tempdir(), {
    got <- read_regexp(input = file)
    expect_identical(got, file)
  })

  #Using regexp
  withr::with_dir(tempdir(), {
    got <- read_regexp(input = paste0(regexp, "/.*\\.R"))
    expect_equal(length(got),9)
  })

  # A non-existing file
  withr::with_dir(tempdir(), {
    read_regexp(input = paste0(regexp, "/donotexsist.R")) |>
    expect_error()
  })

})

covr::report(
  x = covr::file_coverage("R/internal_run.R", "tests/testthat/test-internal_run.R"),
  browse = interactive()
)
