strace_info <- function(path = "strace.log") {
  read_strace_info(
    path = path,
    p_wd = getwd(),
    strace_discards = zephyr::get_option("track_files_discards", "whirl"),
    strace_keep = getwd()
  )
}

test_that("strace works", {
  # skip_on_cran()
  # skip_on_os(c("windows", "mac", "solaris"))

  if (Sys.getenv("CI") != "") {
    cat("Running in CI environment\n")
    cat("ptrace_scope:", readLines("/proc/sys/kernel/yama/ptrace_scope"), "\n")
    cat("Kernel version:", system("uname -r", intern = TRUE), "\n")

    # Safe user info
    tryCatch({
      if (exists("Sys.getuid", mode = "function")) {
        cat("User ID:", Sys.getuid(), "\n")
      }
      if (exists("Sys.geteuid", mode = "function")) {
        cat("Effective user ID:", Sys.geteuid(), "\n")
      }
    }, error = function(e) cat("Error getting user info:", e$message, "\n"))
  }

  withr::with_tempdir(
    code = {
      cat("=== Test Setup ===\n")
      cat("Working directory:", getwd(), "\n")
      cat("this is a dummy file to check strace", file = "dummy.txt")
      cat("Created dummy.txt\n")

      p <-  callr::r_session$new()
      cat("Created R session with PID:", p$get_pid(), "\n")

      # Start strace
      strace_file <- file.path(getwd(), "strace.log")
      cat("Starting strace, output file:", strace_file, "\n")
      start_strace(pid = p$get_pid(), file = strace_file)

      # Check if strace file was created
      Sys.sleep(1)  # Give strace time to start
      if (file.exists(strace_file)) {
        cat("✓ Strace log file created\n")
      } else {
        cat("✗ Strace log file NOT created\n")
      }

      cat("\n=== Test 1: Save mtcars.rds ===\n")
      p$run(\() {
        cat("About to save mtcars.rds in:", getwd(), "\n")
        saveRDS(object = mtcars, file = "mtcars.rds")
        cat("Saved mtcars.rds, file exists:", file.exists("mtcars.rds"), "\n")
        return(getwd())
      })

      Sys.sleep(1)  # Give strace time to capture

      # Check what strace captured
      if (file.exists(strace_file)) {
        strace_content <- readLines(strace_file)
        cat("Strace log has", length(strace_content), "lines\n")
        if (length(strace_content) > 0) {
          cat("First few lines of strace:\n")
          cat(paste(head(strace_content, 10), collapse = "\n"), "\n")
        }
      }

      test <-  strace_info()
      cat("strace_info() returned:\n")
      cat("- write operations:", length(test$write$file), "\n")
      cat("- read operations:", length(test$read$file), "\n")
      cat("- delete operations:", length(test$delete$file), "\n")

      if (length(test$write$file) > 0) {
        cat("Write files found:\n")
        cat(paste(test$write$file, collapse = "\n"), "\n")
      }

      # Test the assertion
      mtcars_found <-  any(grepl(x = test$write$file, pattern = "mtcars.rds"))
      cat("mtcars.rds found in write operations:", mtcars_found, "\n")

      if (!mtcars_found) {
        cat("DEBUG: Looking for any RDS-related activity:\n")
        if (file.exists(strace_file)) {
          rds_lines <- grep("rds", readLines(strace_file), ignore.case = TRUE, value = TRUE)
          if (length(rds_lines) > 0) {
            cat("RDS-related lines in strace:\n")
            cat(paste(head(rds_lines, 5), collapse = "\n"), "\n")
          } else {
            cat("No RDS-related lines found in strace\n")
          }
        }
      }

      testthat::expect_true(mtcars_found)

      cat("\n=== Test 2: Read dummy.txt ===\n")
      p$run(\() {
        content <-  readLines("dummy.txt")
        cat("Read dummy.txt, content:", content, "\n")
        return(content)
      })

      Sys.sleep(1)
      test <- strace_info()

      cat("After reading dummy.txt:\n")
      cat("- read operations:", length(test$read$file), "\n")
      if (length(test$read$file) > 0) {
        cat("Read files found:\n")
        cat(paste(test$read$file, collapse = "\n"), "\n")
      }

      dummy_read_found <-  any(grepl(x = test$read$file, pattern = "dummy.txt"))
      cat("dummy.txt found in read operations:", dummy_read_found, "\n")

      testthat::expect_true(any(grepl(x = test$write$file, pattern = "mtcars.rds")))
      testthat::expect_true(dummy_read_found)

      cat("\n=== Test 3: Delete dummy.txt ===\n")
      p$run(\() {
        result <- file.remove("dummy.txt")
        cat("Removed dummy.txt, success:", result, "\n")
        return(result)
      })

      Sys.sleep(1)
      test <- strace_info()

      cat("After deleting dummy.txt:\n")
      cat("- delete operations:", length(test$delete$file), "\n")
      if (length(test$delete$file) > 0) {
        cat("Delete files found:\n")
        cat(paste(test$delete$file, collapse = "\n"), "\n")
      }

      dummy_delete_found <- any(grepl(x = test$delete$file, pattern = "dummy.txt"))
      cat("dummy.txt found in delete operations:", dummy_delete_found, "\n")

      testthat::expect_true(dummy_delete_found)

      cat("\n=== Cleanup ===\n")
      p$kill()
      cat("Killed R session\n")
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
