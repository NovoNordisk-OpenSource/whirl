
# Example runs calling the logging package --------------------------------


run_script(
  script = retrieve_fpath("prgQmd.qmd"),
  strace = FALSE,
  renv = FALSE,
  cleanup = TRUE,
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
