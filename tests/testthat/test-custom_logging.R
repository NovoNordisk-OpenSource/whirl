test_that("stream to console outside whirl context", {
  log_read("test_read") |>
    expect_output(
      regexp = "\\{\"time\":\".*\",\"type\":\"read\",\"file\":\"test_read\"\\}"
    )

  log_write("test_write") |>
    expect_output(
      regexp = "\\{\"time\":\".*\",\"type\":\"write\",\"file\":\"test_write\"\\}" # nolint: line_length_linter
    )

  log_delete("test_delete") |>
    expect_output(
      regexp = "\\{\"time\":\".*\",\"type\":\"delete\",\"file\":\"test_delete\"\\}" # nolint: line_length_linter
    )
})


test_that("stream to log file in a whirl context", {
  tmp_log_file <- withr::local_tempfile(fileext = ".json")

  withr::with_envvar(
    c(WHIRL_LOG_MSG = tmp_log_file),
    {
      log_read("test_read")
      log_write("test_write")
      log_delete("test_delete")

      x <- read_from_log()

      expect_equal(nrow(x), 3)
      expect_equal(x$type, c("read", "write", "delete"))
      expect_equal(x$file, c("test_read", "test_write", "test_delete"))
    }
  )

  split_log(x) |>
    expect_length(3) |>
    lapply(expect_s3_class, "data.frame") |>
    vapply(\(x) x[["file"]], character(1)) |>
    expect_equal(
      c(read = "test_read", write = "test_write", delete = "test_delete")
    )

  split_log(x[-2, ]) |>
    expect_length(3) |>
    lapply(expect_s3_class, "data.frame") |>
    vapply(\(x) x[["file"]], character(1)) |>
    expect_equal(
      c(read = "test_read", write = "No files", delete = "test_delete")
    )

  x["time"] <- as.POSIXct("2000-01-01 01:01:01")
  knit_print.whirl_log_info(x) |>
    expect_snapshot()
})
