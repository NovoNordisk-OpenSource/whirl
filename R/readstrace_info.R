#' Get session info
#'
#' Retrieve session info and add quarto info if not already there
#'
#' @param path a character vector with path name
#'
#' @export

readstrace_info <- function(path){

  strace_discards <-
    c("/lib",
      "/etc",
      "/lib64",
      "/usr",
      "/var",
      "/opt",
      "/sys" ,
      "/proc",
      "/tmp",
      "/.$")

  data_strace <-
    whirl::read_strace(path, strace_discards = strace_discards) |> dplyr::tibble()

  file_actions <- whirl::refine_strace(data_strace)[c("input", "output")]

  class(file_actions) <- c("whirl_strace_info", class(file_actions))
  for (i in seq_along(file_actions)) {
    class(file_actions[[i]]) <- c(paste0("whirl_strace_", names(file_actions)[[i]]), class(file_actions[[i]]))
  }

  return(file_actions)
}


#' @noRd

knit_print.whirl_strace_info <- function(x, ...){

  x |>
    lapply(knitr::knit_print)
}

#' @noRd

knit_print.whirl_strace_input <- function(x, ...){

   x |>
    dplyr::select("file", "duration", "time") |>
    knitr::kable(caption = "Input") |>
    knitr::knit_print()

}

#' @noRd

knit_print.whirl_strace_output <- function(x, ...){

  x |>
    dplyr::select("file", "duration", "time") |>
    knitr::kable(caption = "Output") |>
    knitr::knit_print()

}


