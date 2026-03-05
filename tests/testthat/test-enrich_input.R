test_that("Enrich input works as expected", {
  # Find all R programs

  enriched <- test_script("_whirl_r_programs.yaml") |>
    enrich_input() |>
    expect_type("list") |>
    expect_length(1)

  enriched[[1]] |>
    expect_type("list") |>
    expect_length(2) |>
    expect_named(c("name", "paths")) |>
    lapply(expect_type, "character") |>
    invisible()

  # Unnamed steps

  test_script("_whirl_unnamed.yaml") |>
    enrich_input() |>
    expect_type("list") |>
    expect_length(3) |>
    vapply(FUN = \(x) x$name, FUN.VALUE = character(1)) |>
    expect_equal(
      c("Named step", "Step 2", "Step 3")
    )

  # File input

  enriched <- test_script("success.R") |>
    enrich_input() |>
    expect_type("list") |>
    expect_length(1)

  enriched[[1]] |>
    expect_type("list") |>
    expect_length(2) |>
    expect_named(c("name", "paths")) |>
    unlist() |>
    expect_equal(
      c(
        name = "Step 1",
        paths = test_script("success.R")
      )
    )

  # Pruning a config file

  test_script("_whirl.yaml") |>
    enrich_input(steps = "Second step") |>
    expect_type("list") |>
    expect_length(1) |>
    vapply(FUN = \(x) x$name, FUN.VALUE = character(1)) |>
    expect_equal("Second step")

  test_script("_whirl.yaml") |>
    enrich_input(steps = c("First step", "Second step")) |>
    expect_type("list") |>
    expect_length(2) |>
    vapply(FUN = \(x) x$name, FUN.VALUE = character(1)) |>
    expect_equal(c("First step", "Second step"))

  # Expected error when input is a directory

  test_script("") |>
    enrich_input() |>
    expect_error()

  # Evaluate expressions in yaml

  test_script("_whirl_expression.yaml") |>
    enrich_input() |>
    expect_type("list") |>
    expect_length(1) |>
    vapply(FUN = \(x) x$name, FUN.VALUE = character(1)) |>
    expect_match(regexp = format(Sys.Date()))
})

test_that("Named list syntax sets step names", {
  success <- test_script("success.R")
  error <- test_script("error.R")

  # Named character element
  enriched <- enrich_input(list("Step A" = success))
  expect_equal(enriched[[1]]$name, "Step A")
  expect_equal(enriched[[1]]$paths, success)

  # Named list with unnamed elements
  enriched <- enrich_input(list("Step B" = list(success)))
  expect_equal(enriched[[1]]$name, "Step B")
  expect_equal(enriched[[1]]$paths, success)

  # Named list with named sub-elements (sub-names stripped by read_glob)
  enriched <- enrich_input(
    list("Step C" = list("Sub A" = success, "Sub B" = error))
  )
  expect_equal(enriched[[1]]$name, "Step C")
  expect_equal(enriched[[1]]$paths, c(success, error))

  # Mixed old and new syntax
  enriched <- enrich_input(list(
    "New Style" = success,
    list(name = "Old Style", paths = error)
  ))
  expect_length(enriched, 2)
  expect_equal(enriched[[1]]$name, "New Style")
  expect_equal(enriched[[1]]$paths, success)
  expect_equal(enriched[[2]]$name, "Old Style")
  expect_equal(enriched[[2]]$paths, error)
})
