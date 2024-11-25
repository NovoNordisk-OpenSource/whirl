test_that("read_regexp() finds the right files", {

  # A single file

  test_script("success.R") |>
    read_regexp() |>
    expect_equal(test_script("success.R"))

  # All R files in a directory

  test_script("") |>
    file.path("*.R") |>
    read_regexp() |>
    expect_match("\\.R$") |>
    length() |>
    expect_gt(1)

  # Error when file does not exist

  test_script("") |>
    file.path("fake_program.R") |>
    read_regexp() |>
    expect_error()

})
