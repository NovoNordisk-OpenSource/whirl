test_that("Approved packages", {
  check_approved("pkg@version") |>
    expect_type("logical") |>
    expect_length(1) |>
    is.na() |>
    expect_true()

  check_approved(character(), "pkg@version") |>
    expect_type("logical") |>
    expect_length(0)

  check_approved(
    used = c("pkg1@1.0.0", "pkg2@5.6.7"),
    approved = c("pkg1@1.0.0", "pkg2@4.3.2")
  ) |>
    expect_type("logical") |>
    expect_equal(c(TRUE, FALSE))
})
