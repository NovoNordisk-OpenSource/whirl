test_that("consistent output from renv help functions", {

  withr::local_options("usethis.quiet" = TRUE)

  tmpdir <- withr::local_tempdir()

  usethis::create_project(path = tmpdir, open = FALSE)

  usethis::local_project(tmpdir)

  status <- renv_status() |>
    expect_s3_class(c("whirl_renv_status"))

  knit_print_whirl_renv_status(status) |>
    as.character() |>
    expect_equal("::: {.callout-warning}\n## renv not used\n:::")

  status$status$lockfile$Packages <- c("a", "b")

  knit_print_whirl_renv_status(status) |>
    as.character() |>
    expect_match(
      "::: \\{.callout-important collapse=true\\}\n## renv out of sync"
    )

  status$status$synchronized <- TRUE

  knit_print_whirl_renv_status(status) |>
    as.character() |>
    expect_match("::: \\{.callout-tip collapse=true\\}\n## renv synchronized")
})
