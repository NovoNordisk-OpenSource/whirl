#' Report renv status in a callout block
#' @export

renv_callout <- function(){

  renv_message <- capture.output(renv_status <- renv::status())

  if (!length(renv_status$lockfile$Packages)){
    renv_note <- "warning"
    renv_title <- "renv not used"
    renv_message <- NULL
  } else if (renv_status$synchronized) {
    renv_note <- "tip"
    renv_title <- "renv synchronized"
  } else {
    renv_note <- "important"
    renv_title <- "renv out of sync"
  }

  quarto_callout(
    text = renv_message,
    title = renv_title,
    type = renv_note,
    collapse = if (!is.null(renv_message)) {TRUE} else {NULL}
  )
}
