#' Get session info
#'
#' Retrieve session info and add quarto info if not already there
#'
#' @noRd

session_info <- function(approved_folder_pkgs, approved_url_pkgs) {
  info <- sessioninfo::session_info()

  if (!is.null(approved_folder_pkgs) |
    !is.null(approved_url_pkgs)) {
    info$packages <- check_approved(
      approved_pkg_folder = approved_folder_pkgs,
      approved_pkg_url = approved_url_pkgs,
      session_pkgs = info$packages
    )
    class(info$packages) <- c("approved_pkgs", class(info$packages))
  } else {
    info$packages
    class(info$packages) <- c("packages_info", class(info$packages))
  }

  info$environment <- Sys.getenv() |>
    as.list() |>
    unlist(recursive = F) |>
    tibble::enframe(name = "Setting", value = "Value")
  class(info$environment) <- c("environment_info", class(info$environment))

  info$options <- options()
  info$options <- info$options[!names(info$options) %in% "rl_word_breaks"]
  class(info$options) <- c("options_info", class(info$options))

  # TODO: Extend to also cover external and python below in methods.
  info[!names(info) %in% c("platform", "packages", "environment", "options")] <- NULL

  if (is.null(info$platform$quarto)) {
    quarto_path <- Sys.getenv("QUARTO_PATH")
    if (!nzchar(quarto_path)) quarto_path <- unname(Sys.which("quarto"))

    if (nzchar(quarto_path)) {
      quarto_version <- system2(quarto_path, "--version", stdout = TRUE)

      info$platform$quarto <- paste(quarto_version, "@", normalizePath(quarto_path, winslash = "/"))
    }
  }

  class(info) <- c("whirl_session_info", class(info))
  for (i in seq_along(info)) {
    class(info[[i]]) <- c(paste0("whirl_", class(info[[i]])[[1]]), class(info[[i]]))
  }

  return(info)
}


#' @noRd

knit_print.whirl_session_info <- function(x, ...) {
  x |>
    lapply(knitr::knit_print)
}

#' @noRd

knit_print.whirl_platform_info <- function(x, ...) {
  data.frame(
    Setting = names(x),
    Value = x |>
      lapply(paste0, collapse = ", ") |>
      unlist() |>
      unname()
  ) |>
    knitr::kable() |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    knitr::knit_print()
}

#' @noRd

knit_print.whirl_packages_info <- function(x, ...) {
  data.frame(
    Package = x$package,
    Version = x$loadedversion,
    `Date (UTC)` = x$date,
    Source = x$source,
    check.names = FALSE
  ) |>
    knitr::kable() |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    knitr::knit_print()
}

#' @noRd
#'
knit_print.whirl_approved_pkgs <- function(x, ...) {
  hold <- x |>
    data.frame(
      check.names = FALSE
    ) |>
    dplyr::rename(
      Package = .data[["package"]],
      Version = .data[["loadedversion"]],
      `Date (UTC)` = .data[["date"]],
      Source = .data[["source"]]
    )

  row.names(hold) <- NULL
  ncols <- ncol(hold)

  # Select columns whose names start with 'Approved' and
  # Use the columns to determine color of the row

  hold |>
    knitr::kable() |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    kableExtra::column_spec(1:ncols, background = ifelse(as.integer(rowSums(as.matrix(hold[, grepl("^Approved", colnames(hold))]) == "No") == ncol(as.matrix(hold[, grepl("^Approved", colnames(hold))]))) == 1, "orange", "white")) |>
    knitr::knit_print()
}

#' @noRd
insert_at_intervals_df <- function(df, column_name, char_to_insert, interval) {
  df[[column_name]] <- sapply(df[[column_name]], function(input_string) {
    if (nchar(input_string) < interval) {
      return(input_string)
    } else {
      result <- input_string
      insert_positions <- seq(interval, nchar(input_string), by = interval)
      for (i in length(insert_positions):1) {
        result <- paste(substr(result, 1, insert_positions[i] - 1), char_to_insert, substr(result, insert_positions[i], nchar(result)), sep = "")
      }
      return(result)
    }
  })
  return(df)
}

#' @noRd



knit_print.whirl_environment_info <- function(x, ...) {
  dropped_info <-
    c(
      "BASH_FUNC",
      "_SSL_CERT",
      "PRIVATE_KEY",
      "PUBLIC_KEY",
      "SIGNING_KEY"
    )

  other_sensitive <- c("_PAT")

  x |>
    dplyr::filter(!(grepl(
      paste0("(?=.*", dropped_info, ")", collapse = "|"),
      .data$Setting,
      perl = TRUE
    )) & !grepl(sprintf(
      "(%s)$",
      paste0(
        other_sensitive,
        collapse = "|"
      )
    ), .data$Setting)) |>
    insert_at_intervals_df(
      column_name = "Setting",
      char_to_insert = "<br>",
      interval = 45
    ) |>
    insert_at_intervals_df(
      column_name = "Value",
      char_to_insert = "<br>",
      interval = 45
    ) |>
    knitr::kable(escape = F) |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    knitr::knit_print()
}

#' @noRd
knit_print.whirl_options_info <- function(x, ...) {
  data.frame(t(sapply(unlist(x), c))) |>
    tidyr::pivot_longer(tidyselect::everything(),
      values_to = "Value",
      names_to = "Setting"
    ) |>
    knitr::kable() |>
    kableExtra::kable_styling(
      bootstrap_options = "striped",
      full_width = TRUE
    ) |>
    knitr::knit_print()
}
