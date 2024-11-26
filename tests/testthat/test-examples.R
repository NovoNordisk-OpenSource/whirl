test_that("All example scripts run with consistent output", {

  withr::with_tempdir({

    # Copy all example scripts to the temporary working directory

    system.file("examples", package = "whirl") |>
      list.files(full.names = TRUE) |>
      file.copy(recursive = TRUE, to = ".")

    # Run all examples after in separate steps

    res <- list.files() |>
      as.list() |>
      run() |>
      expect_no_error() |>
      expect_no_warning()

    # Unify result to only be about the status of the script and without
    # the full path to the script

    res$script <- basename(res$script)
    res$result <- lapply(res$result, \(x) x$status)

    # Check that the results now are consistent
    expect_snapshot_value(res, style = "json2")
  })
})
