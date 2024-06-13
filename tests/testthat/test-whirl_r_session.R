
p <- whirl_r_session$new()
p$use_strace()
p$run_script("prg1.R")
#> debug message
p$create_log()
p$create_outputs()


a <- p$get_dir() |> print()

dir.exists(a)
a |> list.files()

p$run_something()


p$kill()
p$finalize()
rm(p)
gc()
dir.exists(a)


# Pseudo use inside run_script()

p <- whirl_r_session$new()
on.exit(p$kill())

whirl_r_session

p

p$set_inputs()
p$set_dir()
p$set_verbosity_level()
p$start_strace()
p$render_script()
p$read_strace()
p$render_log()
p$report_status()
p$create_outputs()

p$call(\() Sys.sleep(10))
p$wait(timeout = 1)
p$read()


p <- whirl:::whirl_r_session$new()
