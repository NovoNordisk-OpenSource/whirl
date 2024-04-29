
test_that("R script works", {

  script <- system.file("examples/prg1.R", package = "whirl")

  run_script(script = script) |>
    expect_invisible()

  run_script(script = script, check_renv = TRUE) |>
    expect_invisible()

  run_script(script = script, check_renv = FALSE) |>
    expect_invisible()

  run_script(script = script, out_dir = tempdir()) |>
    expect_invisible()

  skip_on_os(c("windows", "mac", "solaris"))

  run_script(script = script, track_files = TRUE, check_renv = TRUE) |>
    expect_invisible()

  run_script(script = script, track_files = TRUE, check_renv = FALSE) |>
    expect_invisible()

  withr::with_options(
    new = list(whirl.track_files_discards = c("/lib", "/etc", "/lib64")),
    code = {

      run_script(script = script, track_files = TRUE, check_renv = TRUE) |>
        expect_invisible()

      run_script(script = script, track_files = TRUE, check_renv = FALSE) |>
        expect_invisible()

    })

})

