test_that("fails with invalid input", {
  data.frame(dummy = "dummy") |>
    util_queue_summary() |>
    expect_error()
})

test_that("Summary tibble is created successfully", {
  skip_if_no_quarto()

  cat("\n=== DEBUGGING util_queue_summary TEST ===\n")

  # Check Python availability first
  cat("1. Checking Python availability...\n")
  python_available <-  tryCatch({
    py_available <- reticulate::py_available(initialize = TRUE)
    cat(sprintf("   Python available: %s\n", py_available))
    if (py_available) {
      py_config <- reticulate::py_config()
      cat(sprintf("   Python executable: %s\n", py_config$python))
      cat(sprintf("   Python version: %s\n", py_config$version))
    }
    py_available
  }, error = function(e) {
    cat(sprintf("   Python check failed: %s\n", e$message))
    FALSE
  })

  q <- whirl_queue$new(n_workers = 1, verbosity_level = "quiet")

  # Choose scripts based on Python availability
  if (python_available) {
    cat("2. Using both R and Python scripts\n")
    scripts <- test_script(c("success.R", "py_success.py"))
    expected_rows <- 2
  } else {
    cat("2. Python not available, using only R script\n")
    scripts <- test_script("success.R")
    expected_rows <- 1
  }

  # Debug script paths
  cat("3. Script information:\n")
  for (i in seq_along(scripts)) {
    cat(sprintf("   Script %d: %s\n", i, scripts[i]))
    cat(sprintf("   Exists: %s\n", file.exists(scripts[i])))
    if (file.exists(scripts[i])) {
      cat(sprintf("   Size: %d bytes\n", file.size(scripts[i])))
      # Show first few lines
      lines <- readLines(scripts[i], n = 3, warn = FALSE)
      cat(sprintf("   First lines: %s\n", paste(lines, collapse = " | ")))
    }
  }

  cat("4. Starting queue execution...\n")
  start_time <- Sys.time()

  # Run with error handling
  tryCatch({
    result <- scripts |> q$run()
    end_time <-  Sys.time()
    cat(sprintf("   Execution completed in %.2f seconds\n",
                as.numeric(end_time - start_time, units = "secs")))
  }, error = function(e) {
    cat(sprintf("   ERROR during execution: %s\n", e$message))
    cat(sprintf("   Error class: %s\n", class(e)[1]))
    stop(e)
  })

  cat("5. Queue state after execution:\n")
  cat(sprintf("   Queue length: %d\n", nrow(q$queue)))

  # Debug each queue item
  for (i in seq_len(nrow(q$queue))) {
    cat(sprintf("   Item %d:\n", i))
    item <- q$queue[i, ]

    # Basic info
    cat(sprintf("     Script: %s\n", basename(item$script)))
    cat(sprintf("     Status: %s\n", item$status))

    # Working directory info
    if (!is.null(item$session[[1]])) {
      tryCatch({
        wd <- item$session[[1]]$.__enclos_env__$private$wd
        cat(sprintf("     Working dir: %s\n", wd))
        cat(sprintf("     WD exists: %s\n", dir.exists(wd)))

        if (dir.exists(wd)) {
          files <- list.files(wd, full.names = FALSE)
          cat(sprintf("     Files in WD: %s\n",
                      if(length(files) > 0) paste(files, collapse = ", ") else "none"))

          # Check for specific files
          result_file <-  file.path(wd, "result.rds")
          log_file <- file.path(wd, "log.html")
          error_file <- file.path(wd, "error.txt")

          cat(sprintf("     result.rds exists: %s\n", file.exists(result_file)))
          cat(sprintf("     log.html exists: %s\n", file.exists(log_file)))
          cat(sprintf("     error.txt exists: %s\n", file.exists(error_file)))

          # Check log for errors if it exists
          if (file.exists(log_file)) {
            log_content <- readLines(log_file, warn = FALSE)
            error_lines <- grep("error|Error|ERROR|failed|Failed|FAILED",
                                log_content, ignore.case = TRUE, value = TRUE)
            if (length(error_lines) > 0) {
              cat("     Log errors found:\n")
              for (err_line in head(error_lines, 3)) {
                cat(sprintf("       %s\n", substr(err_line, 1, 100)))
              }
            } else {
              cat("     No errors found in log\n")
            }
          }

          # Check error file if it exists
          if (file.exists(error_file)) {
            error_content <-  readLines(error_file, warn = FALSE)
            cat("     Error file content:\n")
            for (err_line in head(error_content, 5)) {
              cat(sprintf("       %s\n", err_line))
            }
          }
        }
      }, error = function(e) {
        cat(sprintf("     Error accessing session info: %s\n", e$message))
      })
    } else {
      cat("     Session is NULL\n")
    }
  }

  cat("6. Testing util_queue_summary...\n")

  # Test the summary function with error handling
  summary_result <- tryCatch({
    summary_tbl <- q$queue |> util_queue_summary()
    cat(sprintf("   Summary created successfully\n"))
    cat(sprintf("   Summary class: %s\n", paste(class(summary_tbl), collapse = ", ")))
    cat(sprintf("   Summary dimensions: %d x %d\n", nrow(summary_tbl), ncol(summary_tbl)))
    cat(sprintf("   Summary columns: %s\n", paste(names(summary_tbl), collapse = ", ")))

    # Show summary content
    if (nrow(summary_tbl) > 0) {
      cat("   Summary content:\n")
      for (i in seq_len(nrow(summary_tbl))) {
        cat(sprintf("     Row %d: Tag=%s, Status=%s, Filename=%s\n",
                    i,
                    summary_tbl$Tag[i],
                    summary_tbl$Status[i],
                    summary_tbl$Filename[i]))
      }
    }

    summary_tbl
  }, error = function(e) {
    cat(sprintf("   ERROR in util_queue_summary: %s\n", e$message))
    cat(sprintf("   Error traceback:\n"))
    traceback_lines <-  capture.output(traceback())
    for (line in head(traceback_lines, 10)) {
      cat(sprintf("     %s\n", line))
    }
    stop(e)
  })

  cat("7. Running test assertions...\n")

  # Test assertions with better error messages
  tryCatch({
    summary_result |>
      expect_s3_class("tbl_df")
    cat("   ✓ Class assertion passed\n")
  }, error = function(e) {
    cat(sprintf("   ✗ Class assertion failed: %s\n", e$message))
    stop(e)
  })

  tryCatch({
    summary_result |>
      expect_named(c("Tag", "Directory", "Filename", "Status", "Hyperlink", "Information"))
    cat("   ✓ Column names assertion passed\n")
  }, error = function(e) {
    cat(sprintf("   ✗ Column names assertion failed: %s\n", e$message))
    cat(sprintf("   Expected: %s\n", paste(c("Tag", "Directory", "Filename", "Status", "Hyperlink", "Information"), collapse = ", ")))
    cat(sprintf("   Actual: %s\n", paste(names(summary_result), collapse = ", ")))
    stop(e)
  })

  tryCatch({
    actual_rows <- nrow(summary_result)
    expect_equal(actual_rows, expected_rows)
    cat(sprintf("   ✓ Row count assertion passed (%d rows)\n", actual_rows))
  }, error = function(e) {
    cat(sprintf("   ✗ Row count assertion failed: %s\n", e$message))
    cat(sprintf("   Expected: %d rows\n", expected_rows))
    cat(sprintf("   Actual: %d rows\n", nrow(summary_result)))
    stop(e)
  })

  cat("=== TEST COMPLETED SUCCESSFULLY ===\n\n")
})
