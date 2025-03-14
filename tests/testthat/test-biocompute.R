input_yml <- test_script("_whirl_biocompute.yaml")

queue <- whirl::run(input = input_yml, out_formats = NULL, track_files = TRUE)

create_io_domain(queue) |>
  expect_no_condition() |>
  expect_snapshot()

create_execution_domain(queue) |>
  expect_no_condition()


create_parametrics_domain(input_yml)

create_biocompute(queue, input_yml) |>
  write_biocompute(pretty = TRUE)
