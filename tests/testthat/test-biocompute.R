
queue <- whirl::run(input = test_script(c("biocompute.R", "biocompute.R")),
                    out_formats = NULL
)

create_io_domain(queue) |>
  expect_no_condition() |>
  expect_snapshot()

create_execution_domain(queue) |>
  expect_no_condition()

create_parametrics_domain(test_script("_whirl_biocompute.yaml"))

queue$result[[1]]$session_info_rlist$log_info.read



create_biocompute(queue, test_script("_whirl_biocompute.yaml")) |>
  write_biocompute(pretty = TRUE)




x <- whirl::run(input = test_script("_whirl_biocompute.yaml"),
                out_formats = NULL
)



io_domain <- create_io_domain(queue)

execution_domain <- create_execution_domain(queue)



parametric_domain <- create_parametrics_domain(execution_overview)

biocompute_json <- create_bco(
  execution_domain,
  parametric_domain,
  io_domain
)

# Write the JSON data to a file
write(biocompute_json, "biocompute/biocompute.json")
