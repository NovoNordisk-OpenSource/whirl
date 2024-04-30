test_that("strace works", {

  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir({

    p <- callr::r_session$new()

    start_strace(pid = p$get_pid(), file = "strace")

    p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

    p$close()

    strace_info <- readstrace_info(path = "strace", strace_discards = options::opt("track_files_discards"))
  })

  strace_info$input |>
    nrow() |>
    expect_equal(0)

  strace_info$output$file |>
    expect_equal("mtcars.rds")

})
