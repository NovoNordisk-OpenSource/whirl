#' Use whirl
#'
#' @description
#'
#' Utility function to setup execution with whirl in your project:
#'
#' 1. Creates configuration file (default `_whirl.yaml`)
#' 1. Updates `.gitignore` to not include log files
#'
#' See `vignette("whirl")` for how to specify paths inside the
#' configuration file.
#'
#' @param config_file Path to the whirl config file, relative to the project
#' @export

use_whirl <- function(config_file = "_whirl.yaml") {
  cli::cli_h1("Setup {.pkg whirl}")

  rlang::check_installed("usethis")

  usethis::use_git_ignore(ignores = "*_log.(html|json|md)")

  config <- system.file("use_whirl/_whirl.yaml", package = "whirl") |>
    readLines()

  config_file_path <- usethis::proj_path(config_file)
  usethis::write_over(path = config_file_path, lines = config)
  usethis::edit_file(path = config_file_path)

  cli::cli_alert_info("Run project with {.run whirl::run(\"{config_file}\")}")

  cli::cli_h1("")

  return(invisible(config_file))
}
