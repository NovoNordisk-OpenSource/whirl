#' Converting input to a standardized list with that contain step names and
#' files paths
#'
#' @param input Can be a vector, a list or a whirl config file.
#' @param steps A filter argument for selecting specific steps that should be
#'   executed
#' @return A list
#' @noRd
enrich_input <- function(input, # nolint: cyclocomp_linter
                         steps = NULL) {
  # Characterize the input
  is_config_file <-
    !is.list(input) &&
    any(grepl("yaml|yml", get_file_ext(input)))
  is_character <- is.character(input)

  # Read yaml and extract list
  if (is_config_file && length(input) == 1) {
    root_dir <- dirname(input)
    config_whirl <- yaml::read_yaml(file = input, eval.expr = TRUE)
    got <- config_whirl$"steps"
  } else {
    root_dir <- getwd()
  }

  # Convert vector to list
  if (is_character && !is_config_file) {
    got <- list(input)
  }

  if (is.list(input)) {
    got <- input
  }

  names <- list()
  paths <- list()
  for (i in seq_along(got)) {
    # Identify the step names - if none, then create a default name
    check_name <- any(grepl("name", names(got[[i]])))
    if (check_name) {
      names[[i]] <- got[[i]][[which(grepl("name", names(got[[i]])))]]
    } else {
      names[[i]] <- paste0("Step ", i)
    }

    # Identify the paths
    check_path <- any(grepl("path", names(got[[i]])))
    if (check_path) {
      paths[[i]] <- got[[i]][[which(grepl("path", names(got[[i]])))]]
    } else {
      paths[[i]] <- got[[i]]
    }
  }

  # Normalizing the paths and read regexp
  for (j in seq_along(paths)) {
    normalized <- unlist(lapply(
      X = paths[[j]],
      FUN = normalize_with_base,
      base = root_dir
    ))
    paths[[j]] <- read_glob(normalized)
  }

  # If input include one or more directories
  if (any(unlist(lapply(paths, dir.exists)))) {
    show <- vector()
    for (i in seq_along(paths)) {
      show <- c(show, paths[[i]][unlist(lapply(paths[[i]], dir.exists))])
    }
    show <- unlist(show)
    cli::cli_abort("The input argument in run() does not accept directories,
        please specify which file(s) in {.val {show}} that should be executed")
  }

  # Merge the names and paths into a list
  out <- mapply(list,
    "name" = names,
    "paths" = paths,
    SIMPLIFY = FALSE
  )

  # Get the step names
  step_names <- unlist(out)[grepl("name", names(unlist(out)))]

  # Prune the list when steps have been selected
  if (!is.null(steps)) {
    id <- which(step_names %in% steps)
    # Update the vector of names
    step_names <- step_names[id]
    # Update the list
    out <- out[id]
  }

  # Output the steps that will be executed
  message_ <- c(
    "The following steps will be executed",
    step_names |> rlang::set_names("*")
  )

  zephyr::msg_verbose(message = message_, msg_fun = cli::cli_inform)

  invisible(out)
}
