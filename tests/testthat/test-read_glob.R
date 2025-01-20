test_that("testing read_glob()", {
  # A single file
  test_script("success.R") |>
    read_glob() |>
    expect_equal(test_script("success.R"))

  # All R files in a directory
  test_script("") |>
    file.path("*.R") |>
    read_glob() |>
    expect_match("\\.R$") |>
    length() |>
    expect_gt(1)

  # Error when file does not exist
  test_script("") |>
    file.path("fake_program.R") |>
    read_glob() |>
    expect_message()
})
