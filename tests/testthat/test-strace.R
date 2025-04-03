strace_info <- function(path = "strace.log") {
  read_strace_info(
    path = path,
    p_wd = getwd(),
    strace_discards = zephyr::get_option("track_files_discards", "whirl"),
    strace_keep = getwd()
  )
}

test_that("strace works", {
  skip_on_ci()
  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir(
    code = {
      cat("this is a dummy file to check strace", file = "dummy.txt")

      p <- callr::r_session$new()

      start_strace(pid = p$get_pid(), file = file.path(getwd(), "strace.log"))

      # Only save a file

      p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

      test <- strace_info()

      any(grepl(x = test$write$file, pattern = "mtcars.rds")) |>
        testthat::expect_true()

      # Also read dummy.txt

      p$run(\() readLines("dummy.txt"))
      test <- strace_info()
      any(grepl(x = test$write$file, pattern = "mtcars.rds")) |>
        testthat::expect_true()
      any(grepl(x = test$read$file, pattern = "dummy.txt")) |>
        testthat::expect_true()

      # Finally delete read dummy.txt

      p$run(\() file.remove("dummy.txt"))

      test <- strace_info()
      any(grepl(x = test$delete$file, pattern = "dummy.txt")) |>
        testthat::expect_true()

      p$kill()
    },
    tmpdir = getwd()
  )
})
