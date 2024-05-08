#' @noRd
mdformats <- function(log_html, mdfmt, out_dir) {

    newname <- gsub(
    pattern = "\\.[^\\.]*$",
    replacement = "",
    x = basename(log_html)
  )

  if (length(mdfmt) > 1) {
    newname <- paste0(newname, "_", mdfmt)
  }

  newname <- paste0(newname, ".md")

  for (i in seq_along(newname)) {
    knitr::pandoc(
      input = log_html,
      format = mdfmt[[i]],
      ext = "md"
    )

    file.copy(
      from = file.path(
        tempdir(),
        gsub(
          pattern = "\\.[^\\.]*$",
          replacement = ".md",
          x = basename(log_html)
        )
      ),
      to = file.path(
        out_dir,
        newname[[i]]
      ),
      overwrite = TRUE
    )
  }
}
