test_that("testing enrich_input()", {
  file_config <- system.file("examples/demo/metadata/metadata_whirl.yaml", package = "whirl")
  incomplete_config <- system.file("examples/demo/demo_2_whirl.yml", package = "whirl")
  file <- system.file("examples/demo/adam/mk100adsl.R", package = "whirl")
  config_to_prune <- system.file("examples/demo/demo_whirl.yaml", package = "whirl")
  directory <- system.file("examples/simple", package = "whirl")

  #A config file
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = file_config)
    expect_type(enriched, "list")
    names(enriched[[1]]) |> expect_equal(c("name", "paths"))
  })

  #Incomplete config
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = incomplete_config)
    expect_type(enriched, "list")
    names(enriched[[1]]) |> expect_equal(c("name", "paths"))
    enriched[[1]]$name |> expect_equal("Step 1: Unnamed chunk")
    enriched[[2]]$name |> expect_equal("Step 2: Unnamed chunk")
  })

  #A file
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = file)
    expect_type(enriched, "list")
    names(enriched[[1]]) |> expect_equal(c("name", "paths"))
    enriched[[1]]$name |> expect_equal("Step 1: Unnamed chunk")
    enriched[[1]]$paths |> expect_equal(file)
  })

  #Pruning a config file
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = config_to_prune, steps = "Run ADaM mk100 programs")
    expect_type(enriched, "list")
    names(enriched[[1]]) |> expect_equal(c("name", "paths"))
    length(enriched) |> expect_equal(1)
  })

  #Pruning a config file
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = config_to_prune,
                             steps = c("Run ADaM mk100 programs",
                                       "Run ADaM mk300 programs"))
    expect_type(enriched, "list")
    names(enriched[[1]]) |> expect_equal(c("name", "paths"))
    length(enriched) |> expect_equal(2)
  })

  #Expect error as directory is given as input
  withr::with_dir(tempdir(), {
    enriched <- enrich_input(input = directory) |>
      expect_error()
  })

})



