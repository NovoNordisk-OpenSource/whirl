test_that("Biocompute object created correctly", {
  input_yml <- test_script("_whirl_biocompute.yaml")
  queue <- whirl::run(
    input = input_yml,
    out_formats = NULL,
    summary_file = NULL
  )

  create_io_domain(queue) |>
    expect_snapshot_value()

  create_execution_domain(queue) |>
    expect_no_condition()

  create_parametrics_domain(
    config = yaml::read_yaml(input_yml),
    base_path = dirname(input_yml)
  ) |>
    expect_no_condition()

  write_biocompute(queue)
})
