test_that("quarto error", {
  skip_if_no_quarto()

  test_script("render_error.R") |>
    run(summary_file = NULL) |>
    expect_error(
      regexp = "object 'params' not found"
    )
})
