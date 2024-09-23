.onLoad <- function(...) {
  vctrs::s3_register("knitr::knit_print", "whirl_renv_status")
  vctrs::s3_register("knitr::knit_print", "whirl_session_info")
  vctrs::s3_register("knitr::knit_print", "whirl_platform_info")
  vctrs::s3_register("knitr::knit_print", "whirl_packages_info")
  vctrs::s3_register("knitr::knit_print", "whirl_approved_pkgs")
  vctrs::s3_register("knitr::knit_print", "whirl_environment_info")
  vctrs::s3_register("knitr::knit_print", "whirl_options_info")
  vctrs::s3_register("knitr::knit_print", "whirl_log_info")
  vctrs::s3_register("knitr::knit_print", "whirl_summary_info")
}
