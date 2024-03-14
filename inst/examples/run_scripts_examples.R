
# Example runs calling the logging package --------------------------------

# Define function to delete logs
# Delete existing logs
rm_files <- function(file){
  if (file.exists(file)) {
    #Delete file if it exists
    file.remove(file)
  }
}

rm_files("/scer/homedirs/lvgk/test_output/prgQmd.html")

run_script(
  script = retrieve_fpath("prgQmd.qmd"),
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/prgQmd.html" |> browseURL()

rm_files("/scer/homedirs/lvgk/test_output/box_plot.html")
run_script(
  script = "/scer/homedirs/lvgk/training/nn1234/nn1234-0002/current/stats/program/statprog/box_plot.R",
  strace = TRUE,
  renv = FALSE,
  cleanup = FALSE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/box_plot.html" |> browseURL()


rm_files("/scer/homedirs/lvgk/test_output/prg1.html")
run_script(
  retrieve_fpath("prg1.R"),
  strace = TRUE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/prg1.html" |> browseURL()

rm_files("/scer/homedirs/lvgk/test_output/prgRmd.html")
run_script(
  retrieve_fpath("prgRmd.Rmd"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
  output_dir = "../test_output"
)

"/scer/homedirs/lvgk/test_output/prgRmd.html" |> browseURL()

