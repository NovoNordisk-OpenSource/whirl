strace_info <- function(path = "strace.log") {
  read_strace_info(
    path = path,
    p_wd = getwd(),
    strace_discards = zephyr::get_option("track_files_discards", "whirl"),
    strace_keep = getwd()
  )
}

test_that("strace works", {
  skip_on_cran()
  #skip_on_ci()
  skip_on_os(c("windows", "mac", "solaris"))

  if (Sys.getenv("CI") != "") {
    cat("Running in CI environment\n")
    cat("ptrace_scope:", readLines("/proc/sys/kernel/yama/ptrace_scope"), "\n")
    cat("Kernel version:", system("uname -r", intern = TRUE), "\n")

    # Use safer methods to get user info
    tryCatch({
      if (exists("Sys.getuid", mode = "function")) {
        cat("User ID:", Sys.getuid(), "\n")
      } else {
        cat("Sys.getuid() not available\n")
      }
    }, error = function(e) cat("Error getting UID:", e$message, "\n"))

    tryCatch({
      if (exists("Sys.geteuid", mode = "function")) {
        cat("Effective user ID:", Sys.geteuid(), "\n")
      } else {
        cat("Sys.geteuid() not available\n")
      }
    }, error = function(e) cat("Error getting EUID:", e$message, "\n"))

    # Alternative ways to get user info
    cat("USER env var:", Sys.getenv("USER"), "\n")
    cat("LOGNAME env var:", Sys.getenv("LOGNAME"), "\n")
    cat("whoami output:", system("whoami", intern = TRUE), "\n")
    cat("id output:", system("id", intern = TRUE), "\n")

    # Test strace directly
    cat("Testing strace on simple command:\n")
    strace_test <-  system("timeout 2s strace -e trace=write echo 'test' 2>&1", intern = TRUE)
    cat("Strace test result:\n")
    cat(paste(strace_test, collapse = "\n"), "\n")
  }

  withr::with_tempdir(
    code = {
      cat("this is a dummy file to check strace", file = "dummy.txt")

      p <-  callr::r_session$new()

      start_strace(pid = p$get_pid(), file = file.path(getwd(), "strace.log"))

      # Only save a file
      p$run(\() saveRDS(object = mtcars, file = "mtcars.rds"))

      test <- strace_info()

      any(grepl(x = test$write$file, pattern = "mtcars.rds")) |>
        testthat::expect_true()

      # Also read dummy.txt
      p$run(\() readLines("dummy.txt"))
      test <-  strace_info()
      any(grepl(x = test$write$file, pattern = "mtcars.rds")) |>
        testthat::expect_true()
      any(grepl(x = test$read$file, pattern = "dummy.txt")) |>
        testthat::expect_true()

      # Finally delete read dummy.txt
      p$run(\() file.remove("dummy.txt"))

      test <-  strace_info()
      any(grepl(x = test$delete$file, pattern = "dummy.txt")) |>
        testthat::expect_true()

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
