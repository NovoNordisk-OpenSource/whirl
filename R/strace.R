#' Start strace
#' @param pid pid
#' @param file file
#' @export

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
#' @import dplyr
#'
#' @export

readstrace_info <- function(path, strace_discards = NULL){
  if (is.null(strace_discards)){
    strace_discards <- c(
      "/lib",
      "/etc",
      "/lib64",
      "/usr",
      "/var",
      "/opt",
      "/sys" ,
      "/proc",
      "/tmp",
      "/.$",
      "/null",
      "/urandom",
      "/.cache",
      "./file",
      .libPaths()
    )
  }

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

#' Read STRACE file
#'
#' @param strace_discards characters to identify records to discard
#' @param path Path to the .strace file
#'
#' @return A `tibble` with strace information.
#' @export
#'
#' @importFrom tibble `%>%`
#' @importFrom tidyr separate


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
      into = c("pid", "time", "rawfile"),
      fill = "right",
      extra = "merge",
      remove = TRUE
    ) %>%
    tidyr::separate(
      .data$rawfile,
      sep = "\\)\\s*= ",
      into = c("rawfile", "num"),
      remove = FALSE
    ) %>%
    tidyr::separate(
      .data$rawfile,
      sep = '\\", ',
      into = c("rawfile", "what"),
      remove = FALSE,
      fill = "right"
    ) %>%
    tidyr::separate(
      .data$what,
      sep = ", ",
      into = c("what", "access"),
      remove = FALSE,
      fill = "right"
    ) %>%
    tidyr::separate(
      .data$num,
      sep = " <",
      into = c("num", "duration"),
      remove = FALSE,
      fill = "right"
    )

  data_strace$entrynum <- seq_len(nrow(data_strace))
  data_strace$rawfile <- gsub('\\"', "", data_strace$rawfile)

  data_strace <- data_strace %>%
    mutate(
      file = stringr::str_remove(
        stringr::str_remove(rawfile, "openat\\(AT_FDCWD,"),
        "chdir\\("
      ),
      type = ifelse(
        grepl("chdir", rawfile),
        "chdir",
        ifelse(grepl("unlink", rawfile), "unlink", "other")
      )
    )

  relative_files <-
    data_strace[!grepl("/", data_strace$file) &
                  data_strace$type != "chdir", "entrynum"]
  chdirs <-
    c(0, data_strace[grepl("/", data_strace$file) &
                       data_strace$type == "chdir", "entrynum"])
  rel_chdirs <-
    c(0, data_strace[!grepl("/", data_strace$file) &
                       data_strace$type == "chdir", "entrynum"])

  max.ch <- 0
  rel_dirs <- list()
  file_paths <- vector()
  for (i in seq_along(relative_files)) {
    pos <- chdirs[chdirs < relative_files[i]]
    max.ch[i] <- pos[length(pos)]
    rel_dirs[[i]] <-
      rel_chdirs[max.ch[i] < rel_chdirs &
                   rel_chdirs < relative_files[i]]
    chdirs <- chdirs[chdirs >= max.ch[i]]

    if (max.ch[i] == 0) {
      file_paths[i] <-
        paste(c(getwd(), data_strace$file[c(rel_dirs[[i]], relative_files[i])]), collapse = "/")
    } else {
      file_paths[i] <-
        paste(data_strace$file[c(max.ch[i], rel_dirs[[i]], relative_files[i])], collapse = "/")
    }
  }

  data_strace$file[relative_files] <- file_paths


  data_strace <-
    data_strace[grep(paste(strace_discards, collapse = "|"),
                     data_strace$file,
                     invert = TRUE), ]

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
#' @export

refine_strace <- function(data_strace) {
  # remove empty lines and folders
  data_strace <- data_strace %>%
    filter(trimws(file) != "/" & grepl( "\\.", basename(file)))

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
