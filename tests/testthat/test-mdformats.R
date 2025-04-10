test_that("pandoc works", {
  skip_if_no_quarto()

  x <- whirl_r_session$new()

  file.copy(
    from = test_script("test-mdformats.html"),
    to = file.path(x$get_wd(), "log.html")
  ) |>
    expect_true()

  tmpdir <- withr::local_tempdir()

  mdformats(
    script = "test1.R",
    log_html = file.path(x$get_wd(), "log.html"),
    mdfmt = c("gfm", "commonmark", "markua"),
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
