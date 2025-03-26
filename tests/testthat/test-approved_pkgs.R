test_that("Running approved_pkgs workflow", {
  url <- getOption("repos")
  status <- check_url(sprintf("%s/src/contrib/PACKAGES", url))

  skip_if(!status)

  result <- check_approved(
    approved_pkg_folder = .libPaths(),
    approved_pkg_url = url,
    session_pkgs = sessioninfo::session_info()$packages
  ) |>
    expect_no_error()

  expect_s3_class(result, "packages_info")
})
