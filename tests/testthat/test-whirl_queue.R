
q <- whirl_queue$new(n_workers = 2)

q
q$workers

q$push("inst/examples/simple/prg1.R")

q

q$next_ids
q$available_workers
q$next_workers

q$poll(50)
q$wait()
q$workers
q$queue$result[[1]]$status


################

file.path("inst/examples/simple", c("success.R", "warning.R", "error.R")) |>
  q$push()

q
q$workers
q$poll(50)
q$wait()
q

start <- Sys.time()

list.files("inst/examples/simple", pattern = "^sleep_", full.names = TRUE) |>
  head(2) |>
  q$push()

q$wait()

end <- Sys.time()

q
q$workers

###########################

q$run("inst/examples/simple/prg1.R")

file.path("inst/examples/simple", c("success.R", "warning.R", "error.R")) |>
  q$run()

file.path("inst/examples/simple", c("success.R", "warning.R", "error.R")) |>
  as.list() |>
  q$run()

##############################

plan <- list(
  "inst/examples/simple/prg1.R",
  file.path("inst/examples/simple", c("success.R", "warning.R")),
  list(
    file.path("inst/examples/simple", "error.R"),
    list.files("inst/examples/simple", pattern = "^sleep_", full.names = TRUE) |>
      head(2)
    ),
  "inst/examples/sequence_exuction/error.R"
  )

str(plan)

q$run(plan)
