info <- new.env()


#'@noRd
unlink_whirl_error_file <- function(){
  unlink(
    file.path(
      Sys.getenv("HOME"),
      ".whirl_already_an_error"
    )
  )
}

#'@noRd
create_whirl_error_file <- function(){

  file_ <- file.path(
    Sys.getenv("HOME"),
    ".whirl_already_an_error"
  )
  if(!file.exists(file_)){
    file.create(
      file_
    )
  }

}

#' @noRd
whirl_file_exits <- function(){
  file.exists(
    file.path(
      Sys.getenv("HOME"),
      ".whirl_already_an_error"
    )
  )
}



#' @noRd
#' @importFrom fs file_exists
detect_whirl_file <- function(folder, file = "_whirl.yaml") {
  # check params
  checkmate::assert_directory(folder)

  # name
  file_name <- file.path(folder, file)

  fs::file_exists(file_name)
}

#' Find path for files to log
#'
#' @param step a step from yaml (as a list in R)
#' @param root_dir the root dir of the yaml file
#' @param cli_level Have to be a function from cli
#'
#' @importFrom cli cli_h1
#' @importFrom rlang set_names
#' @importFrom zephyr msg
#'
define_paths <- function(step, root_dir, cli_level = cli::cli_h1){
  # check params
  checkmate::assert_directory(root_dir, access = "rw")
  checkmate::assert_list(step)
  checkmate::assert_function(cli_level)

  # get the message
  msg <- step[["name"]]

  cli_level(msg)

  paths <- step[["paths"]]

  # List files if regexp
  files_ <- lapply(paths, function(x)list.files(root_dir, pattern = x, full.names = TRUE)) |>
    unlist(use.names = FALSE)

  ## Message for debugging
  message_ <- c("i"= "Running logs for files",
                files_  |> rlang::set_names("*"))
  zephyr::msg(
    message_ ,
    msg_fun = cli::cli_inform,
    levels_to_write = "verbose"
  )

  return(files_)
}


#' Logging one step from yaml
#'
#' @inheritParams define_paths
#' @param summary_dir A character string of file path specifying the directory where the summary log will be stored.
#'
one_step_logging <- function(step, root_dir, summary_dir, cli_level = cli::cli_h1){
  # check params
  checkmate::assert_directory(summary_dir, access = "rw")

  ## Find paths for files or folders
  files_with_folder <- define_paths(step, root_dir, cli_level)

  folder_or_not <- fs::is_dir(files_with_folder)
  folders <- names(folder_or_not[folder_or_not])
  files <- names(folder_or_not[!folder_or_not])

  ### init vars
  without_whirl <- character(0)
  list_of_result <- data.frame()

  # If folder then _whirl.yaml or not ?
  if(length(folders) > 0){

    test_whirl <- detect_whirl_file(folders)

    with_whirl <- folders[test_whirl]
    without_whirl <- folders[!test_whirl]

    ## If _whirl.yaml call the function again, create a loop
    if(length(with_whirl) > 0){
      cli::cat_line("")
      cli::cli_alert_info("'_whirl files detected', read and creating logs for it.\n")

      ### What happens if it is not named "_whirl.yaml"
      config_file <- file.path(with_whirl, "_whirl.yaml")
      ### Call logging_from_yaml to create a loop througt nested folders
      list_of_result <- purrr::map(config_file, logging_from_yaml, summary_dir, cli_level = cli::cli_h3 )   |>
        purrr::map(purrr::list_rbind)  |>
        purrr::list_rbind()

      cli::cli_alert_success("Logs created for this config, {config_file}\n")

      cli::cli_inform("Continue current step")

    }
  }

  ## continue with files and folders whitout whirl yaml config
  to_compute <- c(files, without_whirl)

  scripts_ <- log_scripts(to_compute, parallel = TRUE, summary_dir = summary_dir)

  if(any(scripts_$Status == "error")){

    create_whirl_error_file()
  }


  scripts_<- scripts_ |>
    dplyr::mutate(.before = "Status", Name = step[["name"]])

  results <- rbind(list_of_result, scripts_)

  return(results)
}


#' Read and create summary data frame
#'
#' @param file configuration to define steps, have to be a yaml
#' @inheritParams one_step_logging
#'
#' @importFrom purrr map
logging_from_yaml <- function(file, summary_dir, cli_level = cli::cli_h1){
  ## check params
  checkmate::assert_file(file, extension = c("yaml", "yml"))

  ## create logs
  config_whirl <- yaml::yaml.load_file(
    file
  )

  steps <- config_whirl$steps
  root_dir <- dirname(file)

  summary <- purrr::map(steps, one_step_logging, root_dir, summary_dir, cli_level)

  return(summary)
}



#' Create logs for files from a yaml configuration
#'
#' @param file yaml configuration file
#' @param summary_dir A character string of file path specifying the directory where the summary log will be stored.
#'
#' @return A list containing the execution results for each script. Each element of the
#'   list is a character string indicating the success or failure of the script execution.
#' @export
#'
#' @examplesIf FALSE
#'
#' file_ <- system.file(
#'             "examples",
#'             "sequence_exuction",
#'              "_whirl.yaml",
#'              package = "whirl"
#'              )
#' logs_from_whirl_config(file_, tempdir())
logs_from_whirl_config <- function(file, summary_dir = "."){
  # Get the summary df
  ## Setup error as FALSE
  unlink_whirl_error_file()

  summary_df <- logging_from_yaml(file, summary_dir)
  ## Clean up
  unlink_whirl_error_file()

  # define path for the knit_print function
  if (summary_dir == getwd()) {
    summary_dir_f <- here::here()
  } else {
    summary_dir_f <- normalizePath(summary_dir, winslash = "/")
  }

  ######## Finalize it
  summary_qmd <- withr::local_tempfile(lines = readLines(system.file("documents/summary.qmd", package = "whirl")), fileext = ".qmd")
  summary_log_html <- withr::local_tempfile(fileext = ".html")

  rmarkdown::render(
    input = summary_qmd,
    output_format = "html_document",
    output_file = summary_log_html,
    params = list(summary_df = summary_df |> purrr::list_rbind(), summary_dir = summary_dir_f),
    quiet = TRUE
  )

  # Create requested outputs

  file_copy <- tryCatch(
    file.copy(
      from = summary_log_html,
      to = file.path(summary_dir, "summary.html"),
      overwrite = TRUE
    )
  )

}







