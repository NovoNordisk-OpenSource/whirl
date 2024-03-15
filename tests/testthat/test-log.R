test_that("log executes", {
  log_example("prg1.R") |>
    run_script(track_files = FALSE, renv = FALSE) |>
    expect_invisible()
})
