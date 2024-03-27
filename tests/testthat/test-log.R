
test_that("R script works", {

  script <- system.file("examples/prg1.R", package = "whirl")

  run_script(script = script) |>
    expect_invisible()

  run_script(script = script, renv = FALSE) |>
    expect_invisible()

  run_script(script = script, out_dir = tempdir()) |>
    expect_invisible()

  skip_on_os(c("windows", "mac", "solaris"))

  run_script(script = script, track_files = TRUE) |>
    expect_invisible()

  })
