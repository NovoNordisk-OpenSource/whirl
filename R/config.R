#' Function to read and parse the YAML file
#' @importFrom yaml yaml.load_file
#' @noRd
read_yaml_config <- function(yaml_file) {
  config <- yaml::yaml.load_file(yaml_file)
  return(config$params)
}

#' Function to call log_scripts with parameters from YAML file
#' @noRd
execute_with_yaml <- function(yaml_file) {
  params <- read_yaml_config(yaml_file)

  # Call log_scripts with the parameters from the YAML file
  log_scripts(
    paths = trimws(params$paths),
    parallel = params$parallel,
    num_cores = params$num_cores,
    summary_dir = trimws(params$summary_dir),
    track_files = params$track_files,
    check_renv = params$check_renv,
    out_formats = params$out_formats,
    approved_pkgs_folder = params$approved_pkgs_folder,
    approved_pkgs_url = params$approved_pkgs_url,
    out_dir = trimws(params$out_dir)
  )
}
