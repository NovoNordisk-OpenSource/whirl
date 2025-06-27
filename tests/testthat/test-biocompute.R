test_that("Biocompute object created correctly", {
  input_yml <- test_script("_whirl_biocompute.yaml")
  queue <- whirl::run(
    input = input_yml,
    out_formats = NULL,
    summary_file = NULL
  )

  bco_tmp <- withr::local_tempfile(fileext = ".json")

  bco <- write_biocompute(queue = queue, path = bco_tmp) |>
    expect_no_condition()

  expect_snapshot(str(bco$io_domain))
})
