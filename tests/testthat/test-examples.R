test_that("All example scripts run with consistent output", {
  skip_if_no_quarto()

  tmpdir <- withr::local_tempdir()

  # Copy all example scripts to the temporary working directory

  system.file("examples", package = "whirl") |>
    list.files(full.names = TRUE) |>
    file.copy(recursive = TRUE, to = tmpdir)

  res <- list(
    list.files(tmpdir, pattern = "\\.(yaml|yml)$", full.names = TRUE) |>
      as.list(),
    list.files(tmpdir, pattern = "\\.(R|py)$", full.names = TRUE)
  ) |>
    run(summary_file = NULL)

  # Unify result to only be about the status of the script and without
  # the full path to the script

  res$script <- basename(res$script)
  res <- res[c("id", "tag", "script", "status")]

  # Check that the results now are consistent
  expect_snapshot(res)
})
