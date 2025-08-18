#' Helper function to read in all temporary information to be used in the log
#' @noRd
read_info <- function(
  script,
  md,
  start,
  log,
  session,
  environment,
  options,
  python_pip_list = NULL,
  python_new_status = NULL,
  python_old_status = NULL,
  approved_packages = NULL,
  track_files = FALSE
) {
  info <- list(
    script = readRDS(script),
    status = get_status(md = md, start = start),
    files = log |>
      read_from_log(track_files = track_files) |>
      split_log(),
    session = read_session_info(
      file = session,
      approved_packages = approved_packages
    )
  )

  info$session$environment <- read_environment(environment)
  info$session$options <- read_options(options)

  if (!is.null(python_pip_list) && file.exists(python_pip_list)) {
    info$session$platform <- info$session$platform |>
      dplyr::bind_rows(
        data.frame(
          setting = "python",
          value = python_version()
        )
      )

    info$session$python <- read_python(
      old_status = python_old_status,
      new_status = python_new_status,
      pip_list = python_pip_list
    )

    info$session <-
      info$session[c("platform", "R", "python", "environment", "options")]
  }

  return(info)
}

#' Read and format session info output from `sessioninfo::session_info()`
#' @noRd
read_session_info <- function(file, approved_packages = NULL) {
  info <- readRDS(file)

  platform <- info[["platform"]] |>
    unlist() |>
    tibble::enframe(name = "setting", value = "value")

  r_packages <- info[["packages"]] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      package = .data$package,
      version = .data$loadedversion,
      attached = .data$attached,
      approved = check_approved(
        used = paste(.data$package, .data$version, sep = "@"),
        approved = approved_packages
      ),
      path = .data$loadedpath,
      date = vapply(
        X = .data$package,
        FUN = utils::packageDate,
        FUN.VALUE = Sys.Date(),
        USE.NAMES = FALSE
      ) |>
        as.Date(),
      source = source,
      url = vapply(
        X = .data$package,
        FUN = \(x) {
          utils::packageDescription(x)[["URL"]] |>
            dplyr::coalesce(NA_character_)
        },
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
    ) |>
    dplyr::select(
      "package",
      "version",
      "attached",
      "approved",
      "path",
      "date",
      "source",
      "url"
    )

  list(
    platform = platform,
    R = r_packages
  )
}

#' Read and format list of environment variables from `Sys.getenv()`
#' @noRd
read_environment <- function(file) {
  readRDS(file) |>
    as.list() |>
    unlist(recursive = FALSE) |>
    tibble::enframe(name = "variable", value = "value") |>
    dplyr::filter(
      stringr::str_detect(
        string = .data$variable,
        pattern = paste0(
          zephyr::get_option("environment_secrets", "whirl"),
          collapse = "|"
        ),
        negate = TRUE
      )
    )
}

#' Read and format options output from `options()`
#' @noRd
read_options <- function(file) {
  readRDS(file) |>
    tibble::enframe(name = "option", value = "value") |>
    dplyr::filter(
      !.data$option %in% "rl_word_breaks" # Removed due to breaking tables
    )
}

#' Retrieve python version and path
#' @noRd
python_version <- function() {
  reticulate::py_config()[["version"]] |>
    as.character() |>
    paste("@", reticulate::py_config()[["python"]])
}

#' Read and format python packages information from a JSON file
#' JSON files created in `inst/documents/python_modules.py`.
#' Pip list is created in `ìnst/documents/dummy.qmd`.
#' @noRd
read_python <- function(old_status, new_status, pip_list) {
  old <- old_status |>
    jsonlite::read_json() |>
    lapply(FUN = unlist, use.names = FALSE)

  new <- new_status |>
    jsonlite::read_json() |>
    lapply(FUN = unlist, use.names = FALSE)

  pip <- pip_list |>
    readRDS() |>
    read.table(
      text = _,
      col.names = c("package", "version", "path", "installer")
    ) |>
    tail(-2) |>
    tibble::as_tibble()

  if (!nrow(pip)) {
    return(
      tibble::tibble(
        package = character(0),
        version = character(0),
        path = character(0),
        namespaced = logical(0)
      )
    )
  }

  pip |>
    dplyr::select(-installer) |>
    dplyr::filter(
      package %in%
        c(
          setdiff(new$namespaced, old$namespaced),
          setdiff(new$loaded, old$loaded)
        )
    ) |>
    dplyr::mutate(
      namespaced = package %in% new$namespaced
    )
}
