read_info <- function(
  script,
  md,
  start,
  log,
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

read_session_info <- function(file, approved_packages = NULL) {
  info <- readRDS(file)

  platform <- info[["platform"]] |>
    unlist() |>
    tibble::enframe(name = "setting", value = "value")

  r_packages <- info[["packages"]] |>
    tibble::as_tibble() |>
    dplyr::transmute(
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
    )

  list(
    platform = platform,
    R = r_packages
  )
}

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

read_options <- function(file) {
  readRDS(file) |>
    tibble::enframe(name = "option", value = "value") |>
    dplyr::filter(
      !.data$option %in% "rl_word_breaks" # Removed due to breaking tables
    )
}

#' @noRd
python_version <- function() {
  reticulate::py_config()[["version"]] |>
    as.character() |>
    paste("@", reticulate::py_config()[["python"]])
}

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
