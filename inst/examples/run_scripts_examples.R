
# Example runs calling the logging package --------------------------------

#Define the file name that will be deleted
fn <- "/scer/homedirs/lvgk/test_output/prgQmd.html"
fn <- "/scer/homedirs/lvgk/test_output/box_plot.html"
#Check its existence
if (file.exists(fn)) {
  #Delete file if it exists
  file.remove(fn)
}

run_script(
  script = retrieve_fpath("prgQmd.qmd"),
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/prgQmd.html" |> browseURL()

run_script(
  script = "/scer/homedirs/lvgk/training/nn1234/nn1234-0002/current/stats/program/statprog/box_plot.R",
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/box_plot.html" |> browseURL()

run_script(
  retrieve_fpath("prg1.R"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "./inst/output"
)

"/scer/homedirs/lvgk/test_output/prg1.html" |> browseURL()

run_script(
  retrieve_fpath("prgRmd.Rmd"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "./inst/output"
)

"/scer/homedirs/lvgk/test_output/prgRmd.html" |> browseURL()

