.onLoad <- function(...) {

  options::define_options(

    "track files",
    track_files = FALSE,

    "strace discards keywords to use to discard not required lines",
    track_files_discards = "",

    "renv",
    check_renv = FALSE
  )

  vctrs::s3_register("knitr::knit_print", "whirl_renv_status")
  vctrs::s3_register("knitr::knit_print", "whirl_session_info")
  vctrs::s3_register("knitr::knit_print", "whirl_platform_info")
  vctrs::s3_register("knitr::knit_print", "whirl_packages_info")
  vctrs::s3_register("knitr::knit_print", "whirl_environment_info")
  vctrs::s3_register("knitr::knit_print", "whirl_options_info")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_info")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_input")
  vctrs::s3_register("knitr::knit_print", "whirl_strace_output")
}
