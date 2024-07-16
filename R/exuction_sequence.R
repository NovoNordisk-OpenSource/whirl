detect_whirl_file <- function(folder, file = "_whirl.yaml") {
  file_name <- file.path(folder, file)

  fs::file_exists(file_name)
}

define_paths <- function(step, root_dir, cli_level = cli::cli_h1){
  msg <- step[["name"]]

  cli_level(msg)

  paths <- step[["paths"]]

  # List files if regexp
  files_ <- lapply(paths, function(x)list.files(root_dir, pattern = x, full.names = TRUE)) %>% unlist(use.names = FALSE)

  ## Message for debugging
  message_ <- c("i"= "Running logs for files",
                files_ %>%  rlang::set_names("*"))
  zephyr::msg(
    message_ ,
    msg_fun = cli::cli_inform,
    levels_to_write = "verbose"
  )

  return(files_)
}


one_step_logging <- function(step, root_dir, summary_dir, cli_level = cli::cli_h1){

  ## Find paths for files or folders
  files_with_folder <- define_paths(step, root_dir, cli_level)

  folder_or_not <- fs::is_dir(files_with_folder)
  folders <- names(folder_or_not[folder_or_not])
  files <- names(folder_or_not[!folder_or_not])

  ### init vars
  without_whirl <- character(0)
  list_of_result <- data.frame()

  if(length(folders) > 0){

    test_whirl <- detect_whirl_file(folders)

    with_whirl <- folders[test_whirl]
    without_whirl <- folders[!test_whirl]

    ## this function again

    if(length(with_whirl) > 0){
      cli::cat_line("")
      cli::cli_alert_info("'_whirl files detected', read and creating logs for it.\n")


      ### What happens if it is not named "_whirl.yaml"
      config_file <- file.path(with_whirl, "_whirl.yaml")
      list_of_result <- purrr::map(config_file, logging_from_yaml, summary_dir, cli_level = cli::cli_h3 )  %>%
        purrr::map(purrr::list_rbind) %>%
        purrr::list_rbind()

      cli::cli_alert_success("Logs created for this config, {config_file}\n")

      cli::cli_inform("Continue current step")

    }
  }

  to_compute <- c(files, without_whirl)

  test <- log_scripts(to_compute, parallel = TRUE, summary_dir = summary_dir)

  results <- rbind(list_of_result, test)

  return(results)
}

## Read the yaml

logging_from_yaml <- function(file, summary_dir, cli_level = cli::cli_h1){
  config_whirl <- yaml::yaml.load_file(
    file
  )

  steps <- config_whirl$steps
  root_dir <- dirname(file)

  summary <- purrr::map(steps, one_step_logging, root_dir, summary_dir, cli_level)

  return(summary)
}



logs_from_whirl_config <- function(file, summary_dir = "."){
  summary_df <- logging_from_yaml(file, summary_dir)

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
    params = list(summary_df = summary_df %>% purrr::list_rbind(), summary_dir = summary_dir_f),
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







