test_that("interactive whirl R session components not tested in run_script", {
  # p <- whirl_r_session$new(verbose = FALSE)
  #
  # p$print() |>
  #   expect_message()
  #
  # p$get_wd() |>
  #   dir.exists() |>
  #   expect_true()
  #
  # p$get_wd() |>
  #   list.files() |>
  #   sort() |>
  #   expect_equal(c("dummy.qmd", "log.qmd", "mock_config.yaml", "summary.qmd"))
  #
  # p$call(func = Sys.sleep, args = list(time = 5)) # Sleep for 10 second
  #
  # status <- p$wait(timeout = 10)$check_status() # Timeout after 10 ms
  # expect_null(status) # Still running
  #
  # status <- p$wait()$check_status()
  # expect_equal(status$code, 200L) # Completed successfully
  #
  # p$call(func = \() 1 + "a") # Something with an error
  # expect_error(p$wait()$check_status())

  # Test temp dir is deleted correctly
  # dir <- p$get_wd()
  # rm(p)
  # gc()

  #expect_false(dir.exists(dir))
})
