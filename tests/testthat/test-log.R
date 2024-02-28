test_that("log executes", {
  whirl::retrive_fpath("prg1.R") |>
    run_script(renv = FALSE, strace = TRUE, cleanup = TRUE) |>
    expect_invisible()
})
