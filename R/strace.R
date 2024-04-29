#' Start strace
#' @param pid pid
#' @param file file
#' @noRd

start_strace <- function(pid, file) {

  sprintf("strace -f -q -ttt -T -e trace=openat,unlink,unlinkat,chdir -o %s -p %s",
          file,
          pid) |>
    system(wait = FALSE)
}

#' Get session info
#'
#' Retrieve session info and add quarto info if not already there
#'
#' @param strace_discards keywords to use to discard not required lines
#' @param path a character vector with path name
#'
#' @noRd

readstrace_info <- function(path, strace_discards = character()){

  data_strace <- read_strace(path, strace_discards = strace_discards) |>
    dplyr::tibble()

  file_actions <- refine_strace(data_strace)[c("input", "output")]

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
    knitr::kable() |>
    knitr::knit_print()

}

#' @noRd

knit_print.whirl_strace_output <- function(x, ...){

  if (nrow(x) != 0) {
    x |>
      dplyr::select("file", "duration", "time") |>
      knitr::kable() |>
      knitr::knit_print()
  }else{
    dplyr::tibble(file = "No output files") |>
    knitr::kable() |>
    knitr::knit_print()
  }

}

#' Read STRACE file
#'
#' @param strace_discards characters to identify records to discard
#' @param path Path to the .strace file
#'
#' @return A `tibble` with strace information.
#' @noRd

read_strace <- function(path, strace_discards) {
  all_strace <- readLines(path)

  strace_filter <-
    grep("openat\\(AT_FDCWD|unlink\\(|chdir\\(", all_strace, value = TRUE)
  strace_filter <-
    grep(
      "ENOENT \\(No such file or directory\\)|ENXIO \\(No such device or address\\)| ENOTDIR \\(Not a directory\\)",
      strace_filter,
      value = TRUE,
      invert = TRUE
    )

  data_strace <-
    tidyr::separate(
      data.frame(x = strace_filter),
      .data$x,
      sep = '[^,]\\s|\\([a-zA-Z_,\\s]*\\"',
      into = c("pid", "time", "rawfile0"),
      fill = "right",
      extra = "merge",
      remove = TRUE
    ) |>
    dplyr::mutate(rawfile = ifelse(grepl("=", .data$rawfile0), stringr::str_extract(.data$rawfile0, "[^\\)=]+"), .data$rawfile0),
           num = ifelse(grepl("=", .data$rawfile0), sub('.+=(.+)', '\\1', .data$rawfile0), NA)) |>
    tidyr::separate(
      .data$rawfile,
      sep = '\\", ',
      into = c("rawfile", "what"),
      remove = FALSE,
      fill = "right"
    ) |>
    tidyr::separate(
      .data$what,
      sep = ", ",
      into = c("what", "access"),
      remove = FALSE,
      fill = "right"
    ) |>
    tidyr::separate(
      .data$num,
      sep = " <",
      into = c("num", "duration"),
      remove = FALSE,
      fill = "right"
    )

  data_strace_2 <- data_strace |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      entrynum = dplyr::row_number(),
      rawfile = gsub('\\"', "", .data$rawfile)
    )

  data_strace_3 <- data_strace_2 |>
    dplyr::mutate(
      file = stringr::str_remove(
        stringr::str_remove(.data$rawfile, "openat\\(AT_FDCWD,"),
        "chdir\\(") |>
        stringr::str_trim()
      ,
      type = ifelse(
        grepl("chdir", .data$rawfile),
        "chdir",
        ifelse(grepl("unlink", .data$rawfile), "unlink", "other")
        ),

      directory = ifelse(.data$type == "chdir", file, NA_character_) |>
        zoo::na.locf(na.rm = FALSE)
      ) |>
    dplyr::select("time", "duration", "file", "type", "entrynum", "directory", "what", "num", "access")

  data_strace_4 <- data_strace_3 |>
    dplyr::filter(

      # Remove all discards based on file path
      stringr::str_detect(string = .data$file, pattern = paste(strace_discards, collapse = "|"), negate = TRUE),

      # Remove all discards based on current working directory

      stringr::str_detect(string = .data$directory, pattern = paste(strace_discards, collapse = "|"), negate = TRUE) |
        stringr::str_detect(string = .data$file, pattern = "^/"),
      trimws(.data$file) != "/"
    )

  data_strace <- data_strace_4

  if (nrow(data_strace)) {
    data_strace$time <-
      as.POSIXct(as.numeric(data_strace$time), origin = "1970-01-01")
    data_strace$duration <-
      as.numeric(gsub(">", "", data_strace$duration))
    data_strace$action <- NA
    data_strace$action[data_strace$type == "chdir"] <- "Change dir"
    data_strace$action[is.na(data_strace$what) &
                         data_strace$num == 0 &
                         data_strace$type == "unlink"] <- "Deleted"
    data_strace$action[is.na(data_strace$action) &
                         grepl("O_DIRECTORY", data_strace$what)] <-
      "Lookup"
    data_strace$action[is.na(data_strace$action) &
                         is.na(data_strace$access)] <- "Read"
    data_strace$action[is.na(data_strace$action) &
                         !is.na(data_strace$access)] <- "Write"
  }

  # data_strace$file <- gsub(normalizePath("~"), "~", data_strace$file)

  return(data_strace[data_strace$type != "chdir",])
}

#' refine strace output
#'
#' @param data_strace - file lines
#'
#' @return tibble
#' @noRd

refine_strace <- function(data_strace) {
  # remove empty lines and folders
  data_strace <- data_strace |>
    dplyr::filter(.data$directory != ".")

  # remove consecutive duplicates
  rm_dup <-
    rle(paste(data_strace$file, data_strace$num, data_strace$action))
  data_strace_s1 <-
    data_strace[cumsum(c(1, rm_dup$lengths[-length(rm_dup$lengths)])), ]

  # First entry
  data_strace_s1_first <-
    data_strace_s1[!duplicated(data_strace_s1$file), ]

  # last entry
  data_strace_s1_last <-
    data_strace_s1[!duplicated(data_strace_s1$file, fromLast = TRUE), ]

  # Input files: if first entry is read
  input_files <-
    data_strace_s1_first$file[data_strace_s1_first$action == "Read"]

  # Temporary: if write followed by deleted
  writes <- data_strace_s1$file[data_strace_s1$action == "Write"]
  last_deleted <-
    data_strace_s1_last$file[data_strace_s1_last$action == "Deleted"]

  temporary_files <-
    setdiff(intersect(writes, last_deleted), input_files)

  # Output:
  output_files <- setdiff(writes, temporary_files)

  # Deleted
  deleted_files_all <-
    data_strace_s1$file[data_strace_s1$action == "Deleted"]
  deleted_files <-
    setdiff(deleted_files_all,
            c(input_files, temporary_files, output_files))

  input <- data_strace[data_strace$file %in% input_files, ]
  output <-  data_strace[data_strace$file %in% output_files, ]
  temporary <- data_strace[data_strace$file %in% temporary_files, ]
  deleted <- data_strace[data_strace$file %in% deleted_files, ]

  input <- input[!duplicated(input$file), ]
  output <- output[!duplicated(output$file), ]
  temporary <- temporary[!duplicated(temporary$file), ]
  deleted <- deleted[!duplicated(deleted$file), ]

  list(
    input = input,
    output =  output,
    temporary =  temporary,
    deleted =  deleted
  )

}
