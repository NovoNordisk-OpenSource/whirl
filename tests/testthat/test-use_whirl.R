test_that("use_whirl", {
  withr::with_tempdir({
    rlang::local_interactive(FALSE)

    usethis::create_project(path = ".") |>
      expect_message() |>
      suppressMessages()

    use_whirl() |>
      expect_message() |>
      suppressMessages()

    expect_true(file.exists("_whirl.yml"))

    expect_equal(
      readLines("_whirl.yml"),
      readLines(system.file("use_whirl/_whirl.yml", package = "whirl"))
    )

    expect_true(file.exists(".gitignore"))

    expect_contains(
      readLines(".gitignore"),
      "*_log.(html|json|md)"
    )
  })
})
