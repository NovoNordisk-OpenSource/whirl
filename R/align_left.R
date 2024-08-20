#' Left-align text and pad to same length.
#'
#' @param text \code{character} with strings to be left-aligned.
#' @param width \code{numeric} indicating the desired length of strings.
#' @param type \code{character} specifying pre-processing. Trims whitespace from `text` if set to "trimmed".
#' @param sep \code{character} specifying the padding. May give unexpected results if not a single character.
#' @param keep.empty \code{logical} Indicate if empty strings should be kept empty.
#'
#' @return \code{character} with left-aligned text.
align_left <- function(text, width = max(nchar(text)),
                      type = c("non-trimmed", "trimmed"),
                      sep = " ",
                      keep.empty = TRUE) {
  type <- type[1]

  if(type == "trimmed") {
    text <- trimws(text)
  }

  if(nchar(paste(sep, collapse = "")) != 1) {
    warning("Warning: You have supplied a separator value that is not a single character. This may give unexpected results.")
  }

  textWidth <- nchar(text, keepNA = FALSE)
  nspaces   <- floor((width - textWidth))
  if(nspaces < 0 ) {nspaces <- 8}
  spaces    <- sapply(nspaces, function(n) paste(rep(sep, n), collapse = ""))
  r.text    <- paste(text, spaces, sep = "")

  if (keep.empty) {
    r.text[textWidth == 0] <- ""
  }

  return(r.text)
}
