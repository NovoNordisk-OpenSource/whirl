strace_info <- function(path = "strace.log") {
  read_strace_info(
    path = path,
    p_wd = getwd(),
    strace_discards = zephyr::get_option("track_files_discards", "whirl"),
    strace_keep = getwd()
  )
}

wait_for_condition <-  function(check_fn, timeout = 2, interval = 0.1, error_msg = NULL) {
  start_time <- Sys.time()

  while (as.numeric(Sys.time() - start_time) < timeout) {
    tryCatch({
      if (check_fn()) {
        return(TRUE)
      }
    }, error = function(e) {
      # Continue waiting if check_fn() fails
    })

    Sys.sleep(interval)
  }

  # If we get here, the condition was never met
  if (is.null(error_msg)) {
    error_msg <- paste("Condition not met within", timeout, "seconds")
  }

  stop(error_msg, call. = FALSE)
}

check_strace_pattern <-  function(pattern, operation, path = "strace.log") {
  function() {
    test <- strace_info(path = path)
    any(grepl(x = test[[operation]]$file, pattern = pattern))
  }
}


test_that("strace works", {
  skip_on_cran()
  skip_on_os(c("windows", "mac", "solaris"))

  withr::with_tempdir(
    code = {
      cat("this is a dummy file to check strace", file = "dummy.txt")

      p <- callr::r_session$new()
      start_strace(pid = p$get_pid(), file = file.path(getwd(), "strace.log"))

      # Wait for strace to initialize
      wait_for_condition(
        check_fn = function() file.exists("strace.log"),
        error_msg = "strace log file was not created"
      )

      # Test operations
      p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))
      wait_for_condition(
        check_fn = check_strace_pattern("mtcars.rds", "write"),
        error_msg = "mtcars.rds write operation not detected"
      ) |> testthat::expect_true()

      p$run(\() readLines("dummy.txt"))
      wait_for_condition(
        check_fn = check_strace_pattern("dummy.txt", "read"),
        error_msg = "dummy.txt read operation not detected"
      ) |> testthat::expect_true()

      p$run(\() file.remove("dummy.txt"))
      wait_for_condition(
        check_fn = check_strace_pattern("dummy.txt", "delete"),
        error_msg = "dummy.txt delete operation not detected"
      ) |> testthat::expect_true()

      p$kill()
    }
  )
})
test_that("strace fails gracefully OS error handling", {
  skip_on_os("linux")

  # Get current OS
  os_type <- Sys.info()["sysname"]

  # Test for unsupported OS (Windows or Darwin)
  expect_error(
    start_strace(1234, "output.txt"),
    paste("strace does not support", os_type)
  )
})

test_that("strace fails gracefully with mocking error handling", {
  # Define test cases with OS names and expected outcomes
  test_cases <- list(
    list(
      os = "Windows",
      expected = "error",
      message = "strace does not support Windows",
      system_mock = NULL # Not needed for error cases
    ),
    list(
      os = "Darwin",
      expected = "error",
      message = "strace does not support Darwin",
      system_mock = NULL # Not needed for error cases
    ),
    list(
      os = "unknownOS",
      expected = "error",
      message = "strace does not support unknownOS",
      system_mock = NULL # Not needed for error cases
    ),
    list(
      os = "Linux",
      expected = "success",
      message = NULL,
      system_mock = function(...) 0 # Return success (0) for system call
    )
  )

  for (case in test_cases) {
    # Set up OS mock for this test case
    local_mocked_bindings(
      Sys.info = function() {
        c(
          sysname = case$os,
          release = "10.0",
          version = "10.0",
          nodename = paste0("test-", tolower(case$os)),
          machine = "x86_64",
          login = "testuser",
          user = "testuser",
          effective_user = "testuser"
        )
      },
      .package = "base"
    )

    # Set up system mock if provided (for Linux case)
    if (!is.null(case$system_mock)) {
      local_mocked_bindings(
        system = case$system_mock,
        .package = "base"
      )
    }

    # Check expected outcome
    if (case$expected == "error") {
      expect_error(
        start_strace(1234, "output.txt"),
        case$message
      )
    } else if (case$expected == "success") {
      expect_no_error(start_strace(1234, "output.txt"))
    }
  }
})

test_that("strace fails during execution handling", {
  # Mock Sys.info to return Linux
  local_mocked_bindings(
    Sys.info = function() {
      c(
        sysname = "Linux",
        release = "5.4.0",
        version = "5.4.0-generic",
        nodename = "test-linux",
        machine = "x86_64",
        login = "testuser",
        user = "testuser",
        effective_user = "testuser"
      )
    },
    .package = "base"
  )

  # Define test cases: error message and expected error
  test_cases <- list(
    list(
      error = "Operation not permitted",
      expected = "strace cannot attach to process. This may be due to permission errors"
    ),
    list(
      error = "command not found",
      expected = "strace is not installed or not found in PATH"
    ),
    list(
      error = "random_not_defined_error12345",
      expected = "random_not_defined_error12345" # Original error should be propagated
    )
  )

  # Loop through each test case
  for (case in test_cases) {
    # Mock system to throw the specific error
    local_mocked_bindings(
      system = function(...) {
        stop(case$error)
      },
      .package = "base"
    )

    # Test the error handling
    expect_error(
      start_strace(1234, "output.txt"),
      case$expected
    )
  }
})


test_that("refine_strace filters out discarded files", {
  withr::with_tempdir(
    code = {
      # Test data with required seq column
      test_df <- tibble::tibble(
        seq = c(1, 2),
        time = as.POSIXct(c("2023-01-01 10:00:01", "2023-01-01 10:00:02")),
        file = c("/tmp/cache.txt", "/home/important.R"),
        type = c("read", "write")
      )

      # Test: only discards provided (triggers the else if branch)
      result <- refine_strace(test_df,
                              strace_discards = c("tmp"),
                              strace_keep = character())

      expect_equal(result$file, "/home/important.R")
      expect_equal(nrow(result), 1)
    }
  )
})

test_that("read_strace returns empty tibble when file does not exist", {
  withr::with_tempdir(
    code = {
      # Test with non-existent file path
      result <- read_strace("nonexistent_file.log", getwd())

      # Verify empty tibble structure
      expect_s3_class(result, "tbl_df")
      expect_named(result, c("seq", "time", "file", "type"))
      expect_equal(nrow(result), 0)
      expect_equal(ncol(result), 4)

      # Verify column classes
      expect_type(result$seq, "integer")
      expect_s3_class(result$time, "POSIXct")
      expect_type(result$file, "character")
      expect_type(result$type, "character")
    }
  )
})

test_that("read_strace returns empty tibble when file has no relevant content", {
  withr::with_tempdir(
    code = {
      # Create a file with content that gets filtered out (no openat|unlink|chdir)
      cat("some irrelevant log content\nanother line without the required patterns\nyet another line",
          file = "empty_strace.log")

      strace_result <- read_strace("empty_strace.log", getwd())

      # Test the returned data frame structure
      expect_true(tibble::is_tibble(strace_result))
      expect_identical(colnames(strace_result), c("seq", "time", "file", "type"))
      expect_true(nrow(strace_result) == 0)
      expect_true(ncol(strace_result) == 4)

      # Test column data types
      expect_true(is.integer(strace_result$seq))
      expect_true(inherits(strace_result$time, "POSIXct"))
      expect_true(is.character(strace_result$file))
      expect_true(is.character(strace_result$type))
    }
  )
})
