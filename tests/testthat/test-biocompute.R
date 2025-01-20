test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

obje <- whirl::run(input = test_script("biocompute.R"),
  out_formats = "json",
  track_files = TRUE
)

session_info_rlist <- obje$result[[1]]$session_info_rlist

io_domain <- create_io_domain(session_info_rlist)
execution_domain <- create_execution_domain(obje$script, session_info_rlist)
parametric_domain <- create_parametrics_domain()


biocompute_json <- create_bco(
  execution_domain,
  parametric_domain,
  io_domain
)

# Write the JSON data to a file
write(biocompute_json, "biocompute/biocompute.json")
