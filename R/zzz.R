.onLoad <- function(...) { # nocov start
  s3_register("knitr::knit_print", "whirl_renv_status")
  s3_register("knitr::knit_print", "whirl_session_info")
  s3_register("knitr::knit_print", "whirl_platform_info")
  s3_register("knitr::knit_print", "whirl_packages_info")
  s3_register("knitr::knit_print", "whirl_approved_pkgs")
  s3_register("knitr::knit_print", "whirl_environment_info")
  s3_register("knitr::knit_print", "whirl_options_info")
  s3_register("knitr::knit_print", "whirl_log_info")
} # nocov end
