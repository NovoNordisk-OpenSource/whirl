.onLoad <- function(...) {
  vctrs::s3_register("knitr::knit_print", "whirl_renv_status")
}
