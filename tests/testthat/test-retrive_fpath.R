test_that("Path of file is retrieved", {
  retrive_fpath("dummy.qmd") |>
    expect_equal("./inst/documents/dummy.qmd")

  retrive_fpath("dummy_doesnt_exist.qmd") |>
    expect_equal(NA)
})
