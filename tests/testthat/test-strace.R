test_that("strace works", {

  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir({

    p <- callr::r_session$new()

    start_strace(pid = p$get_pid(), file = "strace")

    p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

    p$close()

    strace_info <- read_strace_info(
      path = "strace",
      p_wd = getwd(),
      strace_discards = options::opt("track_files_discards")
      )
  }, tmpdir = getwd())

  strace_info$read$file |>
    expect_equal("No files")

  strace_info$deleted$file |>
    expect_equal("No files")

  strace_info$write$file |>
    expect_match("/mtcars.rds$")

})
