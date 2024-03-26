run_script(
  script = "/scer/homedirs/lvgk/prg11.R",
  track_files = TRUE,
  renv = TRUE,
  out_dir = "/scer/homedirs/lvgk"
)

run_script(
  script = "/scer/homedirs/lvgk/prg11.R",
  track_files = TRUE,
  renv = TRUE,
  strace_discards = "/opt",
  out_dir = "/scer/homedirs/lvgk"
)

run_script(
  script = "inst/examples/prgQmd.qmd",
  track_files = TRUE,
  renv = TRUE,
  out_dir = "/scer/homedirs/lvgk"
)

run_script(
  script = "inst/examples/prgRmd.Rmd",
  track_files = TRUE,
  renv = TRUE,
  out_dir = "/scer/homedirs/lvgk"
)

run_script(
  script = "/scer/homedirs/lvgk/training/nn1234/nn1234-0002/current/stats/program/statprog/box_plot.R",
  track_files = TRUE,
  renv = TRUE,
  out_dir = "/scer/homedirs/lvgk"
)

# "/scer/homedirs/lvgk/prgQmd.strace"
#   all_strace <- readLines("/scer/homedirs/lvgk/prgQmd.strace")
#   data_strace <- data_strace[data_strace$type != "chdir",]

pp <- callr::r_session$new()
strace_log <- withr::local_tempfile(fileext = ".strace")
start_strace(pid = pp$get_pid(), file = strace_log)
pp$close()


as.POSIXlt(1711451188.79446, origin = "1970-01-01")

data_strace$file <- grepl("openat\\(AT_FDCWD, ", "", data_strace$file)

str_remove(string, pattern)

str_remove_all(string, pattern)

data_strace4 <- data_strace %>%
  mutate(file = stringr::str_remove(stringr::str_remove(file, "openat\\(AT_FDCWD,"), "chdir\\("))

stringr::str_replace(data_strace$file, 'openat(AT_FDCWD,', "")

str(data_strace)

stringr::str_remove(data_strace$file, "openat\\(AT_FDCWD, ")
