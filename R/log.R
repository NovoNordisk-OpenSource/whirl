#' Helper function to read in all temporary information to be used in the log
#' @noRd
read_info <- function(
  script,
  md,
  start,
  log,
  pkgs_used,
  session,
  environment,
  options,
  python = NULL,
  approved_packages = NULL
) {
  info <- list(
    script = readRDS(script),
    status = get_status(md = md, start = start),
    files = log |>
      read_from_log() |>
      split_log(),
      session = read_session_info(
        file = session,
        pkgs_used,
        approved_packages = approved_packages
    )
  )

  info$session$environment <- read_environment(environment)
  info$session$options <- read_options(options)

  if (!is.null(python) && file.exists(python)) {
    info$session$platform <- info$session$platform |>
      dplyr::bind_rows(
        data.frame(
          setting = "python",
          value = python_version()
        )
      )
    info$session$python <- read_python(python)
    info$session <-
      info$session[c("platform", "R", "python", "environment", "options")]
  }

  return(info)
}

#' Read and format session info output from `sessioninfo::session_info()`
#' @noRd
read_session_info <- function(file, pkgs_used, approved_packages = NULL) {
  info <- readRDS(file)
  pkgs_used <- readRDS(pkgs_used)

  platform <- info[["platform"]] |>
    unlist() |>
    tibble::enframe(name = "setting", value = "value")

  r_packages <- info[["packages"]] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      package = .data$package,
      version = .data$loadedversion,
      attached = .data$attached,
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
    "path",
    "date",
    "source",
    "url"
  )

  attached <- r_packages |>
    dplyr::filter(r_packages$attached == TRUE)

  if (!identical(pkgs_used$Package, character(0))) {
  as_list <- lapply(
        X = pkgs_used$Package,
        FUN = \(x) {
          sessioninfo::package_info(x, dependencies = FALSE) |>
            as.data.frame() |>
            dplyr::select("package", "version" = "ondiskversion", "date", "source")
        }
      )

  as_dat <- do.call(rbind.data.frame, as_list) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(attached = TRUE)
  } else {
      as_dat <- tibble::tibble()
  }

  directly_used <- dplyr::bind_rows(attached, as_dat) |>
    dplyr::distinct(.data$package, .keep_all = TRUE) |>
    dplyr::arrange(.data$package) |>
    dplyr::mutate(
     approved = check_approved(
        used = paste(.data$package, .data$version, sep = "@"),
        approved = approved_packages
     )
    ) |>
    dplyr::mutate(approved = dplyr::if_else(.data$approved == TRUE, "\u2705 Yes", "\u274C No"
      )
    )

  indirectly_used <- r_packages |>
    dplyr::filter(r_packages$attached != TRUE & !r_packages$package %in% directly_used$package)

  list(
    platform = platform,
    directly_used = directly_used,
    indirectly_used = indirectly_used
  )
}

#' Read and format list of environment variabes from `Sys.getenv()`
#' @noRd
read_environment <- function(file) {
  readRDS(file) |>
    as.list() |>
    unlist(recursive = FALSE) |>
    tibble::enframe(name = "variable", value = "value") |>
    dplyr::filter(
      stringr::str_detect(
        string = .data$variable,
        pattern = paste0(r_secrets(), collapse = "|"),
        negate = TRUE
      )
    )
}

#' List of patterns naming a secret environment variable
#' any match will not be included in the log
#' @noRd
r_secrets <- function() {
  c(
    "BASH_FUNC",
    "_SSL_CERT",
    "_KEY",
    "_KEY",
    "_KEY",
    "_PAT",
    "_TOKEN"
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
#' JSON file created in `inst/documents/python_modules.py`
#' @noRd
read_python <- function(json) {
  json <- jsonlite::fromJSON(json)

  if (!length(json)) {
    return(
      tibble::tibble(
        Package = character(0),
        Version = character(0),
        Path = character(0)
      )
    )
  }

  json |>
    tibble::enframe(name = "Package") |>
    tidyr::unnest_wider(col = "value") |>
    dplyr::rename(
      Version = "version",
      Path = "installation_path"
    )
}
