test_that("Running approved_pkgs workflow", {
  skip_on_cran()
  url <- getOption("repos")[[1]]
  status <- check_url(sprintf("%s/src/contrib/PACKAGES", url))

  skip_if(!status)

  result <- check_approved(
    approved_pkg_folder = .libPaths()[[1]],
    approved_pkg_url = url,
    session_pkgs = sessioninfo::session_info()$packages
  ) |>
    expect_no_error()

  expect_s3_class(result, "packages_info")
})
