test_that("consistent output from renv help functions", {
  withr::local_options("usethis.quiet" = TRUE)

  tmpdir <- withr::local_tempdir()
  usethis::create_project(path = tmpdir, open = FALSE)
  usethis::local_project(tmpdir)

  # When renv is not used
  status <- callr::r(\() renv::status())
  status$synchronized |>
    expect_false()
  status$lockfile$Packages |>
    expect_length(0)
  status$library$Packages |>
    expect_length(0)

  # Initialized and in sync only using renv
  callr::r(\() renv::init())
  status <- callr::r(\() renv::status())

  status$synchronized |>
    expect_true()
  status$lockfile$Packages |>
    expect_length(1) |>
    names() |>
    expect_equal("renv")
  status$library$Packages |>
    names() |>
    expect_contains("renv")

  setdiff(
    x = status$lockfile$Packages,
    y = status$library$Packages
  ) |>
    expect_length(0)

  # a
  writeLines(text = "library(callr)", con = "test.R")
  status <- callr::r(\() renv::status())

  setdiff(
    x = status$lockfile$Packages,
    y = status$library$Packages
  ) |>
    expect_length(0)

  # After snapshot
  callr::r(\() renv::snapshot())
  status <- callr::r(\() renv::status())

  status

  status$synchronized |>
    expect_true()
  expect_equal(
    object = status$library$Packages,
    expected = status$lockfile$Packages
  )
  status$library$Packages
  status$lockfile$Packages
  capture.output(renv::snapshot())
})

callr::r_session$new()
