#' Use whirl
#'
#' @description
#'
#' Utility function to setup execution with whirl in your project:
#'
#' 1. Creates configuration file (default `_whirl.yml`)
#' 1. Updates `.gitignore` to not include log files
#'
#' See `vignette("whirl")` for how to specify paths inside the
#' configuration file.
#'
#' @param config_file Path to the whirl config file, relative to the project
#' @export

use_whirl <- function(config_file = "_whirl.yml") {
  cli::cli_h1("Setup {.pkg whirl}")

  use_template("_whirl.yml", config_file = config_file)

  cli::cli_alert_info("Run project with {.run whirl::run(\"{config_file}\")}")

  cli::cli_h1("")

  return(invisible(config_file))
}

#' Use whirl to create biocompute logs
#'
#' @description
#'
#' Utility function to setup execution with whirl in your project suitable for
#' creating biocompute logs with `write_biocompute()`:
#'
#' 1. Creates configuration file (default `_whirl.yml`) with default values for the `biocompute` metadata.
#' 1. Updates `.gitignore` to not include log files
#'
#' See `vignette("whirl")` for how to specify paths inside the
#' configuration file.
#'
#' @param config_file Path to the whirl config file, relative to the project
#' @param parametrics_file Path to the biocompute parametrics file, relative to the project
#' @export

use_biocompute <- function(config_file = "_whirl.yml", parametrics_file = "_parametrics.yml") {
  cli::cli_h1("Setup {.pkg whirl} and biocompute logs")

  use_template(template = "_biocompute.yml", config_file = config_file)
  use_template(template = "_parametrics.yml", config_file = parametrics_file)

  cli::cli_alert_info(
    "Run project with {.run whirl::run(\"{config_file}\")} and {.run whirl::write_biocompute()}"
  )

  cli::cli_h1("")

  return(invisible(config_file))
}

#' @noRd
use_template <- function(template, config_file) {
  rlang::check_installed("usethis")

  usethis::use_git_ignore(ignores = "*_log.(html|json|md)")

  config <- system.file("use_whirl", template, package = "whirl") |>
    readLines()

  config_file_path <- usethis::proj_path(config_file)
  usethis::write_over(path = config_file_path, lines = config)
  usethis::edit_file(path = config_file_path)
}
