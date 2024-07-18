test_that("test from yaml file", {
  file_ <- system.file("examples", "sequence_exuction", "_whirl.yaml", package = "whirl")

  withr::with_tempdir({
    logs_from_whirl_config(file_) |>
      expect_no_error()
  })

})
