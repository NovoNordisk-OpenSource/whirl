test_that("R script works", {

  script <- "prg1.R"

  withr::with_tempdir(code = {
    file.copy(
      from = system.file("examples", "simple", script, package = "whirl"),
      to = getwd()
    )

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
      }
    )

    withr::with_options(
      new = list(whirl.track_files_discards = character()),
      code = {
        run_script(script = script, track_files = TRUE, check_renv = TRUE) |>
          expect_invisible()

        run_script(script = script, track_files = TRUE, check_renv = FALSE) |>
          expect_invisible()
      }
    )
  })
})
