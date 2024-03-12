
# Example runs calling the logging package --------------------------------


run_script(
  script = retrieve_fpath("prgQmd.qmd"),
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "./inst/output"
)

run_script(
  script = "/scer/homedirs/lvgk/training/nn1234/nn1234-0002/current/stats/program/statprog/box_plot.R",
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "./inst/output"
)



run_script(
  retrieve_fpath("prg1.R"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "./inst/output"
)

run_script(
  retrieve_fpath("prgRmd.Rmd"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "./inst/output"
)


# sprintf("strace -ttt -T -e trace=openat,unlink,unlinkat,chdir -o %s -p %s",
#         paste0(getwd(), ".strace"),
#         get_pid()) |>
#   print() |>
#   system(wait = FALSE)
#
# x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
# cbind(x, sprintf("%a", x), sprintf("%a", y))
