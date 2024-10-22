#' @noRd
mdformats <- function(script, log_html, mdfmt, self, out_dir) {
  newname <- gsub(
    pattern = "\\.[^\\.]*$",
    replacement = "",
    x = basename(script)
  )

  if (length(mdfmt) >= 1) {
    newname <- paste0(newname, "_log_", mdfmt)
  }

  newname <- paste0(newname, ".md")

  for (i in seq_along(newname)) {
    knitr::pandoc(
      input = log_html,
      format = mdfmt[[i]],
      ext = "md"
    )

    file.copy(
      from = file.path(self$get_wd(), "log.md"),
      to = file.path(
        out_dir,
        newname[[i]]
      ),
      overwrite = TRUE
    )
  }
}
