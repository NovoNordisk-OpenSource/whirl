test_that("strace works", {
  skip_on_ci()
  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir(
    code = {
      cat("this is a dummy file to check strace", file = "dummy.txt")

      p <- callr::r_session$new()

      start_strace(pid = p$get_pid(), file = file.path(getwd(), "strace.log"))

      cat("============= Initial: =============", "\n")
      cat(c("wd:", getwd()), "\n")
      cat(c("files:", list.files()), "\n")
      cat(c("environment:", ls()), "\n")
      cat("====================================", "\n")

      # No output yet

      p$run(\() 1 + 1)

      strace_info <- read_strace_info(
        path = "strace.log",
        p_wd = getwd(),
        strace_discards = options::opt("track_files_discards"),
        strace_keep = getwd()
      )

      strace_info$read$file |>
        expect_equal("No files")

      strace_info$delete$file |>
        expect_equal("No files")

      strace_info$write$file |>
        expect_equal("No files")

      # Only save a file

      p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

      strace_info <- read_strace_info(
        path = "strace.log",
        p_wd = getwd(),
        strace_discards = options::opt("track_files_discards"),
        strace_keep = getwd()
      )

      strace_info$read$file |>
        expect_equal("No files")

      strace_info$delete$file |>
        expect_equal("No files")

      strace_info$write$file |>
        expect_match("/mtcars.rds$")

      # Also read dummy.txt

      p$run(\() readLines("dummy.txt"))

      strace_info <- read_strace_info(
        path = "strace.log",
        p_wd = getwd(),
        strace_discards = options::opt("track_files_discards"),
        strace_keep = getwd()
      )

      strace_info$read$file |>
        expect_match("/dummy.txt$")

      strace_info$delete$file |>
        expect_equal("No files")

      strace_info$write$file |>
        expect_match("/mtcars.rds$")

      # Finally delete read dummy.txt

      p$run(\() file.remove("dummy.txt"))

      strace_info <- read_strace_info(
        path = "strace.log",
        p_wd = getwd(),
        strace_discards = options::opt("track_files_discards"),
        strace_keep = getwd()
      )

      strace_info$read$file |>
        expect_match("/dummy.txt$")

      strace_info$delete$file |>
        expect_match("/dummy.txt$")

      strace_info$write$file |>
        expect_match("/mtcars.rds$")

      p$kill()
      p$finalize()

      cat("============= final: =============", "\n")
      cat(c("wd:", getwd()), "\n")
      cat(c("files:", list.files()), "\n")
      cat(c("environment:", ls()), "\n")
      cat("==================================", "\n")
    },
    tmpdir = getwd()
  )
})
