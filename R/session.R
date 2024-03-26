#' Get session info
#'
#' Retrieve session info and add quarto info if not already there
#'
#' @noRd

session_info <- function(){

  info <- sessioninfo::session_info()

  # TODO: Extend to also cover external and python below in methods.
  info[!names(info) %in% c("platform", "packages")] <- NULL

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
    knitr::knit_print()
}

#' @noRd

knit_print.whirl_packages_info <- function(x, ...){

  data.frame(
    Package = x$package,
    Version = x$loadedversion,
    `Date (UTC)` = x$date,
    Source = x$source,
    check.names = FALSE
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}
