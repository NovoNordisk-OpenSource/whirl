#' Run script
#' test with a single script
#' @param script path
#' @param strace logical
#' @param renv logical
#' @param cleanup logical
#' @export

run_script <- function(script, strace = FALSE, renv = TRUE, cleanup = TRUE){

  title <- gsub(pattern = "^.*/", replacement = "", x = script)
  script <- normalizePath(script)
  output <- gsub(pattern = "\\.R$", replacement = "\\.html", x = script)

  x <- rmarkdown::render(
    input = log_document("log.Rmd"),
    params = list(title = title, script = script, strace = strace, renv = renv),
    output_file = output
  )

  # if (cleanup){
  #   list.files(path = "new", pattern = paste0("^", root, "(_files|\\.md|\\.strace)$"), full.names = TRUE) |>
  #     unlink(recursive = TRUE)
  # }

  return(invisible(x))
}

#' Internal log documents
#' @param doc name
#' @export

log_document <- function(doc){
  system.file("documents", doc, package = "whirl")
}

#' Example scripts
#' @param doc name
#' @export

log_example <- function(doc){
  system.file("examples", doc, package = "whirl")
}
