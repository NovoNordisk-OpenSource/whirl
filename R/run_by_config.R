info <- new.env()


#' @noRd
unlink_whirl_error_file <- function() {
  unlink(
    file.path(
      Sys.getenv("HOME"),
      ".whirl_already_an_error"
    )
  )
}

#' @noRd
create_whirl_error_file <- function() {
  file_ <- file.path(
    Sys.getenv("HOME"),
    ".whirl_already_an_error"
  )
  if (!file.exists(file_)) {
    file.create(
      file_
    )
  }
}

#' @noRd
whirl_file_exits <- function() {
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
define_paths <- function(step, root_dir, cli_level = cli::cli_h1) {
  # check params
  checkmate::assert_directory(root_dir, access = "rw")
  checkmate::assert_list(step)
  checkmate::assert_function(cli_level)

  # get the message
  msg <- step[["name"]]

  cli_level(msg)

  paths <- step[["paths"]]

  ## List dirs
  dirs_test <- file.path(root_dir, paths)
  paths_not_dir <- paths[!fs::dir_exists(dirs_test)]
  dirs_ <- dirs_test[fs::dir_exists(dirs_test)]

  if (length(dirs_) == 0) {
    dirs_ <- NULL
  }

  # List files if regexp
  files_ <- lapply(paths_not_dir, function(x) {
    test <- file.path(root_dir, x)
    if (file_exists(test)) {
      return(test)
    } else {
      if (!stringr::str_detect(x, "/")) {
        files <- fs::dir_ls(root_dir, regexp = x, type = "file")
        if (length(files) == 0) {
          cli::cli_abort("No files or folders for this path {x}")
        }
      } else {
        to_construct_path <- as.character(stringr::str_split(x, "/", simplify = TRUE))
        dir_inter <- file.path(root_dir, paste0(to_construct_path[-length(to_construct_path)], collapse = "/"))
        regexp_ <- to_construct_path[length(to_construct_path)]
        files <- fs::dir_ls(dir_inter, regexp = regexp_, type = "file")
      }
      return(files)
    }
  }) |>
    unlist(use.names = FALSE)

  if (length(files_) == 0) {
    files_ <- NULL
  }

  all_contents <- c(dirs_, files_)

  return(all_contents)
}


#' Logging one step from yaml
#'
#' @inheritParams define_paths
#' @param summary_dir A character string of file path specifying the directory
#'   where the summary log will be stored.
#'
one_step_logging <- function(step, summary_dir, root_dir, cli_level = cli::cli_h1) {
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
  if (length(folders) > 0) {
    test_whirl <- detect_whirl_file(folders)

    with_whirl <- folders[test_whirl]
    without_whirl <- folders[!test_whirl]

    ## If _whirl.yaml call the function again, create a loop
    if (length(with_whirl) > 0) {
      cli::cli_alert_info("Additional config file(s) detected, read and execute the individual steps.\n")

      ### What happens if it is not named "_whirl.yaml"
      config_file <- file.path(with_whirl, "_whirl.yaml")
      ### Call logging_from_yaml to create a loop through nested folders
      list_of_result <- purrr::map(config_file,
        logging_from_yaml,
        root_dir = dirname(config_file),
        summary_dir = summary_dir,
        cli_level = cli::cli_h3
      ) |>
        purrr::map(purrr::list_rbind) |>
        purrr::list_rbind()

      cat("\n") #Ensure that the below message appear on a new line
      cli::cli_alert_success("Logs created for this config, {config_file}\n")
    }
  }

  ## continue with files and folders without whirl yaml config
  to_compute <- c(files, without_whirl)
  scripts_ <- NULL

  if (length(to_compute) > 0) {
    if (exists("with_whirl") && length(with_whirl) > 0) {
      cli::cli_inform("Continue current step")
    }

    scripts_ <- run_paths(to_compute,
      parallel = TRUE,
      summary_dir = summary_dir)

    if (any(scripts_$Status == "error")) {
      create_whirl_error_file()
    }

    scripts_ <- scripts_ |>
      dplyr::mutate(.before = "Status", Step = step[["name"]])
  }

  results <- rbind(list_of_result, scripts_)

  return(results)
}


#' Read and create summary data frame
#'
#' @param file configuration to define steps, have to be a yaml
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files is to be executed.
#' @inheritParams one_step_logging
#'
#' @importFrom purrr map

logging_from_yaml <- function(file,
                              summary_dir,
                              root_dir,
                              steps = NULL,
                              cli_level = cli::cli_h2) {
  ## check params
  checkmate::assert_file(file, extension = c("yaml", "yml"))

  ## create logs
  config_whirl <- yaml::yaml.load_file(file)

  steps_list <- config_whirl$steps

  ## Get the step names
  step_names <- unlist(steps_list)[grepl("name", names(unlist(steps_list)))]

  # Prune the list when steps have been selected
  if (!is.null(steps)) {
    id <- which(step_names %in% steps)
    #Update the vector of names
    step_names <- step_names[id]
    #Update the list
    steps_list <- steps_list[id]

  }

  #Output the steps that will be executed
  message_ <- c(
    "The following steps in the config file will be executed",
    step_names |> rlang::set_names("*")
  )

  zephyr::msg(
    message_,
    msg_fun = cli::cli_inform,
    levels_to_write = "verbose"
  )

  summary <- purrr::map(
    steps_list, one_step_logging,
    summary_dir, root_dir, cli_level
  )

  return(summary)
}



#' Create logs for files from a yaml configuration
#'
#' @param file yaml configuration file
#' @param steps An optional argument that can be used if only certain steps
#'   within a config files is to be executed.
#' @param summary_dir A character string of file path specifying the directory
#'   where the summary log will be stored.
#' @param root_dir By default, the root dir of the yaml file.
#'
#' @return A list containing the execution results for each script. Each element
#'   of the list is a character string indicating the success or failure of the
#'   script execution.
#' @export
#'
#' @examplesIf FALSE
#'
#' file_ <- system.file(
#'   "examples",
#'   "sequence_exuction",
#'   "_whirl.yaml",
#'   package = "whirl"
#' )
#' run_by_config(file_, tempdir())
run_by_config <- function(file,
                          steps = NULL,
                          summary_dir = ".",
                          root_dir = dirname(file)) {
  # Get the summary df
  ## Setup error as FALSE before
  unlink_whirl_error_file()


  # On error clean up as well
  summary_df <- try(
    logging_from_yaml(file, steps = steps, summary_dir, root_dir = root_dir),
    silent = TRUE
  )

  if (inherits(summary_df, "try-error")) {
    unlink_whirl_error_file()
    stop(summary_df)
  }


  # define path for the knit_print function
  if (summary_dir == getwd()) {
    summary_dir_f <- here::here()
  } else {
    summary_dir_f <- normalizePath(summary_dir, winslash = "/")
  }

  #Compile the list into a singel dataframe
  summary_df <- summary_df |>
    purrr::list_rbind()

  render_summary(input = summary_df, summary_dir = summary_dir)

  return(invisible(summary_df))

  ## Clean up when it ends
  unlink_whirl_error_file()

}
