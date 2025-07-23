test_that("quarto error", {
  test_script("render_error.R") |>
    run(summary_file = NULL) |>
    expect_error(
      regexp = "object 'params' not found"
    )
})
