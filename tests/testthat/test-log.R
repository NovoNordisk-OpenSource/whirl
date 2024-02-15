test_that("log executes", {

  log_example("prg1.R") |>
    run_script(renv = FALSE, strace = FALSE) |>
    expect_invisible()

})
