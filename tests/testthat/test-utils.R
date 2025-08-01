test_that("get_file_ext()", {
  get_file_ext("file.txt") |>
    expect_equal("txt")

  get_file_ext("a/b/file.a") |>
    expect_equal("a")

  get_file_ext("a/b/file") |>
    expect_equal("")
})

test_that("scale_to_percent()", {
  scale_to_percent(0.5) |>
    expect_equal("50.00%")

  scale_to_percent(0.55124316, 1) |>
    expect_equal("55.1%")
})

test_that("replace_na_with_last()", {
  replace_na_with_last(1:5) |>
    expect_equal(1:5)

  replace_na_with_last(c(1, NA, 3, NA, 5)) |>
    expect_equal(c(1, 1, 3, 3, 5))

  replace_na_with_last(c(NA, NA, NA)) |>
    expect_equal(c(NA, NA, NA))

  replace_na_with_last(c(NA, "a", "b", NA, "c")) |>
    expect_equal(c(NA, "a", "b", "b", "c"))
})

test_that("path_rel()", {
  path_rel("a", "a") |>
    expect_equal(".")

  path_rel("a", "a/b") |>
    expect_equal("..")

  path_rel("a", "a/b/c") |>
    expect_equal("../..")

  path_rel("a/b", "a") |>
    expect_equal("b")

  path_rel("a/b/d", "a/b/c") |>
    expect_equal("../d")
})

test_that("create_cli_links()", {
  text <- create_cli_links("mytext", "my_link") |>
    expect_type("character")

  grep(pattern = "my_link", x = text) |>
    expect_length(0)

  withr::with_options(
    new = list(cli.dynamic = TRUE),
    code = create_cli_links("mytext", "my_link") |>
      expect_type("character") |>
      expect_match("my_link")
  )
})
