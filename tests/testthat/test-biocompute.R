
execution_overview = "~/github-projects/whirl/tests/testthat/scripts/_whirl_biocompute.yaml"
#~/github-projects/whirl/tests/testthat/scripts/_whirl_biocompute.yaml

queue <- whirl::run(input = test_script(execution_overview),
  out_formats = "json",
  track_files = TRUE
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
