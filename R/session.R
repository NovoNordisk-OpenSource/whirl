#' Get session info
#'
#' Retrieve session info and add quarto info if not already there
#'
#' @noRd

session_info <- function(){

  info <- sessioninfo::session_info()

  info$environment <- Sys.getenv() |> as.list() |> unlist(recursive = F) |> tibble::enframe(name= "Setting", value = "Value")
  class(info$environment) <- c("environment_info", class(info$environment))

  info$options <- options()
  info$options <- info$options[!names(info$options)  %in% "rl_word_breaks"]
  class(info$options) <- c("options_info", class(info$options))

  # TODO: Extend to also cover external and python below in methods.
  info[!names(info) %in% c("platform", "packages", "environment", "options")] <- NULL

  if (is.null(info$platform$quarto)){

    quarto_path <- Sys.getenv("QUARTO_PATH")
    if (!nzchar(quarto_path)) quarto_path <- unname(Sys.which("quarto"))

    if (nzchar(quarto_path)){
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

knit_print.whirl_session_info <- function(x, ...){

  x |>
    lapply(knitr::knit_print)
}

#' @noRd

knit_print.whirl_platform_info <- function(x, ...){

  data.frame(
    Setting = names(x),
    Value = x |>
      lapply(paste0, collapse = ", ") |>
      unlist() |>
      unname()
    ) |>
    knitr::kable() |>
    kableExtra::kable_styling(bootstrap_options = "striped"
                              , full_width = TRUE) |>
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
    kableExtra::kable_styling(bootstrap_options = "striped"
                              , full_width = TRUE) |>
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
    c("BASH_FUNC",
      "_SSL_CERT",
      "PRIVATE_KEY",
      "PUBLIC_KEY",
      "SIGNING_KEY")

  x <-
    insert_at_intervals_df(
      x,
      column_name = "Setting",
      char_to_insert = "<br>",
      interval = 45
    ) |>
    insert_at_intervals_df(
      x,
      column_name = "Value",
      char_to_insert = "<br>",
      interval = 45
    ) |>
    dplyr::filter(!(grepl(
      paste0("(?=.*", dropped_info, ")", collapse = "|"),
      .data$Setting,
      perl = TRUE
    ))) |>
    knitr::kable(escape = F) |>
    kableExtra::kable_styling(bootstrap_options = "striped"
                              , full_width = TRUE) |>
    knitr::knit_print()
}

#' @noRd
knit_print.whirl_options_info <- function(x, ...) {
  data.frame(t(sapply(unlist(x), c))) |>
    tidyr::pivot_longer(everything(),
                        values_to = "Value",
                        names_to = "Setting") |>
    knitr::kable() |>
    kableExtra::kable_styling(bootstrap_options = "striped"
                              , full_width = TRUE) |>
    knitr::knit_print()
}

