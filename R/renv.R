#' Get renv status
#' @export

renv_status <- function(){

  msg <- capture.output(status <- renv::status())

  structure(
    list(message = msg, status = status),
    class = c("whirl_renv_status", "list")
    )
}

#' @export

print.whirl_renv_status <- function(x){

  x$message |>
    cat(sep = "\n")

  return(invisible(x))
}

#' @noRd

knit_print.whirl_renv_status <- function(x, ...){

  if (!length(x$status$lockfile$Packages)){
    renv_note <- "warning"
    renv_title <- "renv not used"
    renv_message <- NULL
  } else if (x$status$synchronized) {
    renv_note <- "tip"
    renv_title <- "renv synchronized"
    renv_message <- x$message
  } else {
    renv_note <- "important"
    renv_title <- "renv out of sync"
    renv_message <- renv_message_table(x$message)
  }

  quarto_callout(
    text = renv_message,
    title = renv_title,
    type = renv_note,
    collapse = if (!is.null(renv_message)) {TRUE} else {NULL}
  )

}

#' Format renv message with markdown table
#' @noRd

renv_message_table <- function(renv_message){

  i <- grepl(pattern = "^ +", x = renv_message) |>
    which()

  renv_message[i] <- gsub(pattern = "( |$)(?! )", replacement = "|", x = renv_message[i], perl = TRUE)

  j <- i[[1]]

  c(
    head(renv_message, j),
    gsub(pattern = "[^|]", replacement = "-", x = renv_message[j]),
    tail(renv_message, -j)
  )
}
