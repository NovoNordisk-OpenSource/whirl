#' Get renv status
#' @noRd

renv_status <- function() {
  rlang::check_installed("renv")

  msg <- utils::capture.output(status <- renv::status())

  structure(
    list(message = msg, status = status),
    class = c("whirl_renv_status", "list")
  )
}

#' @noRd

print.whirl_renv_status <- function(x, ...) {
  x$message |>
    cat(sep = "\n")

  return(invisible(x))
}

#' @noRd

knit_print_whirl_renv_status <- function(x, ...) {
  if (!length(x$status$lockfile$Packages)) {
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
    renv_message <- x$message |>
      renv_message_table() |>
      renv_message_headers()
  }

  quarto_callout(
    text = renv_message,
    title = renv_title,
    type = renv_note,
    collapse = if (!is.null(renv_message)) {
      TRUE
    } else {
      NULL
    }
  )
}

#' Format renv message with markdown table.
#' Used when packages are in inconsistent state only.
#' @noRd

renv_message_table <- function(renv_message) {
  i <- grepl(pattern = "^ +", x = renv_message) |>
    which()

  if (!length(i)) {
    return(renv_message)
  }

  renv_message[i] <- gsub(
    pattern = "( |$)(?! )",
    replacement = "|",
    x = renv_message[i],
    perl = TRUE
  )

  j <- i[[1]]

  c(
    utils::head(renv_message, j),
    gsub(pattern = "[^|]", replacement = "-", x = renv_message[j]),
    utils::tail(renv_message, -j)
  )
}

#' Bump renv status headers down to header 3
#' @noRd

renv_message_headers <- function(renv_message) {
  gsub(pattern = "^#", replacement = "###", x = renv_message)
}
