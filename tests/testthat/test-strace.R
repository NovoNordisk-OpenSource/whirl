expect_strace <- function(read, delete, write, path = "strace.log") {
  strace_info <- read_strace_info(
    path = path,
    p_wd = getwd(),
    strace_discards = zephyr::get_option("track_files_discards", "whirl"),
    strace_keep = getwd()
  )

  strace_info$read$file |>
    testthat::expect_match(read)

  strace_info$delete$file |>
    testthat::expect_match(delete)

  strace_info$write$file |>
    testthat::expect_match(write)
}

test_that("strace works", {
  skip_on_ci()
  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir(
    code = {
      cat("this is a dummy file to check strace", file = "dummy.txt")

      p <- callr::r_session$new()

      start_strace(pid = p$get_pid(), file = file.path(getwd(), "strace.log"))

      # No output yet

      p$run(\() 1 + 1)

      expect_strace("^No files$", "^No files$", "^No files$")

      # Only save a file

      p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

      expect_strace("^No files$", "^No files$", "/mtcars.rds$")

      # Also read dummy.txt

      p$run(\() readLines("dummy.txt"))

      expect_strace("/dummy.txt$", "^No files$", "/mtcars.rds$")

      # Finally delete read dummy.txt

      p$run(\() file.remove("dummy.txt"))

      expect_strace("/dummy.txt$", "/dummy.txt$", "/mtcars.rds$")

      p$kill()
      p$finalize()
    },
    tmpdir = getwd()
  )
})
