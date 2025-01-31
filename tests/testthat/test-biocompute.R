
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

