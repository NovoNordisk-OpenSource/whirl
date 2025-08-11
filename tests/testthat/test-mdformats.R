test_that("pandoc works", {
  skip_if_no_quarto()

  x <- whirl_r_session$new()

  file.copy(
    from = test_script("test-mdformats.html"),
    to = file.path(x$get_wd(), "log.html")
  ) |>
    expect_true()

  tmpdir <- withr::local_tempdir()

  # Different pandoc installations support different formats
  # for testing purposes we only use the supported ones here.
  # Error handling tested in new test below.
  mdfmt <- c("gfm", "commonmark", "markua") |>
    intersect(list_pandoc_output_formats())

  mdformats(
    script = "test1.R",
    log_html = file.path(x$get_wd(), "log.html"),
    mdfmt = mdfmt,
    out_dir = tmpdir,
    self = x
  ) |>
    suppressMessages()

  file.path(
    tmpdir,
    paste0("test1_log_", c("gfm", "commonmark", "markua"), ".md")
  ) |>
    file.exists() |>
    all() |>
    expect_true()
})

test_that("gives error when format is not supported", {
  skip_if_no_quarto()

  mdformats(
    script = "script.R",
    log_html = "script.html",
    mdfmt = "fake_format",
    out_dir = "."
  ) |>
    expect_error("not supported by your pandoc installation")
})
