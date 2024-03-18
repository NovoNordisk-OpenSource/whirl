.onLoad <- function(...) {
  vctrs::s3_register("knitr::knit_print", "whirl_renv_status")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_info")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_input")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_output")
}
