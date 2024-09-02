
q <- whirl_queue$new(n_workers = 5)

q
q$workers

q$push("inst/examples/simple/prg1.R")

q$next_ids
q$available_workers
q$next_workers

q$poll()




q$poll()

q$wait()

p <- whirl_r_session$new(verbose = TRUE)
p$
  log_script("inst/examples/simple/prg1.R")$
  wait()$
  check_status()
p$
  create_log()$
  wait()$
  check_status()

p$
  log_finish()$
  create_outputs(out_dir = "inst/examples", format = "html")

rm(p)
gc()
