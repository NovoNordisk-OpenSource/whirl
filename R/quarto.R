#' Create a Quarto callout block with code
#'
#' See https://quarto.org/docs/authoring/callouts.html
#'
#' @param text description
#' @param title description
#' @param type description
#' @param collapse description
#' @noRd
quarto_callout <- function(
    text = NULL,
    title = NULL,
    type = c("note", "warning", "important", "tip", "caution"),
    collapse = NULL) {
  type <- rlang::arg_match(type)
  if (!is.null(collapse)) {
    collapse <- ifelse(collapse, "true", "false")
  }

  c(
    sprintf(
      "::: {.callout-%s}",
      ifelse(!is.null(collapse),
        paste0(type, " collapse=", collapse),
        type
      )
    ),
    if (!is.null(title)) {
      paste("##", title)
    },
    if (!is.null(text)) {
      text
    },
    ":::"
  ) |>
    paste(collapse = "\n") |>
    knitr::asis_output()
}
