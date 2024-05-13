#' Start strace
#' @param pid [integer] process id to attach strace onto
#' @param file [character] path to the file in which to store the strace log
#' @noRd

start_strace <- function(pid, file) {
  sprintf(
    "strace -f -q -ttt -T -e trace=openat,unlink,unlinkat,chdir -o %s -p %s -y",
    file,
    pid
  ) |>
    system(wait = FALSE)
}

#' Get strace info ready for reporting
#'
#' @param path [character] path to the strace log
#' @param p_wd [character] path to the working directory used for the process tracked in strace
#' @param strace_discards [character] keywords to use to discard files from the info
#' @param types [character] which element(s) to report in the info. If not found in strace, a dummy `data.frame` is inserted.
#' @return [list] of `data.frame`(s) of the relevant files for each type of info
#' @noRd

read_strace_info <- function(path, p_wd, strace_discards = character(), strace_keep = character(), types = c("read", "write", "deleted")) {
  strace <- path |>
    read_strace(p_wd = p_wd) |>
    refine_strace(strace_discards = strace_discards, strace_keep = strace_keep)

  class(strace) <- c("whirl_strace_info", class(strace))

  # Split in a tibble for each type of output

  strace <- split(strace[c("time", "file")], strace$type)

  # Add empty table for types not reported

  out <- vector(mode = "list", length = length(types)) |>
    rlang::set_names(types)

  out[names(strace)] <- strace

  i <- lapply(X = out, FUN = is.null) |>
    unlist() |>
    which()
  dummy <- tibble::tibble(file = "No files")
  class(dummy) <- c("whirl_strace_info", class(dummy))
  out[i] <- list(dummy)

  return(out)
}


#' @noRd

knit_print.whirl_strace_info <- function(x, ...) {
  x |>
    knitr::kable() |>
    knitr::knit_print()
}

#' Read STRACE file
#'
#' @param path [character] path to the strace log
#' @param p_wd [character] path to the working directory used for the process tracked in strace
#' @return [data.frame] with strace information where all files are reported with their full path
#' @noRd

read_strace <- function(path, p_wd) {
  strace <- readLines(path) |>
    stringr::str_squish() |>
    stringr::str_subset("openat|unlink|chdir") |>
    stringr::str_subset(
      pattern = "ENOENT \\(No such file or directory\\)|ENXIO \\(No such device or address\\)| ENOTDIR \\(Not a directory\\)",
      negate = TRUE
    ) |>
    stringr::str_subset("<unfinished \\.{3}>|<\\.{3} [a-zA-Z]+ resumed>", negate = TRUE)

  strace_df <- strace |>
    unglue::unglue_data(
      patterns = list(
        "{pid} {time} {funct}({keyword}<{dir}>, \"{path}\", {action}, {access}) = {result}<{result_dir}> <{duration}>",
        "{pid} {time} {funct}({keyword}<{dir}>, \"{path}\", {action}) = {result}<{result_dir}> <{duration}>",
        "{pid} {time} {funct}({keyword}<{dir}>, \"{path}\", {action}) = {result} <{duration}>",
        "{pid} {time} {funct}(\"{path}\") = {result} <{duration}>"
      )
    ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      seq = dplyr::row_number(),
      pid = as.numeric(.data$pid),
      time = as.POSIXct(as.numeric(.data$time), origin = "1970-01-01"),
      result = as.numeric(.data$result),
      duration = as.numeric(.data$duration),
      type = dplyr::case_when(
        .data$funct == "chdir" ~ "chdir",
        stringr::str_detect(.data$funct, "unlink") ~ "delete",
        .data$funct == "openat" & stringr::str_detect(.data$action, "O_DIRECTORY") ~ "lookup",
        .data$funct == "openat" & is.na(.data$access) ~ "read",
        .data$funct == "openat" & !is.na(.data$access) ~ "write",
      ),
      wd = dplyr::case_when(
        .data$type == "chdir" & .data$path == "." ~ p_wd,
        .data$type == "chdir" ~ .data$path
      ) |>
        zoo::na.locf(na.rm = FALSE) |>
        dplyr::coalesce(p_wd),
      dir = dplyr::coalesce(.data$dir, .data$wd)
    )

  # Full paths etc

  strace_df |>
    dplyr::mutate(
      .data$time,
      .data$type,
      file = dplyr::if_else(
        stringr::str_detect(string = .data$path, pattern = "^/", negate = TRUE),
        file.path(.data$dir, .data$path),
        .data$path
      )
    ) |>
    dplyr::filter(.data$type %in% c("read", "write", "delete")) |>
    dplyr::select(.data$seq, .data$time, .data$file, .data$type)
}

#' refine strace output
#'
#' @param strace_df [data.frame] with strace information as returned from `read_strace`
#' @param strace_discards [character] keywords to use to discard files from the info
#' @return [data.frame] with strace information where discarded and duplicate files are removed
#' @noRd

refine_strace <- function(strace_df, strace_discards = character(), strace_keep = character()) {
  # Remove discards if provided

  if (length(strace_discards) & length(strace_keep)) {
    strace_df <- strace_df |>
      dplyr::filter(
        stringr::str_detect(
          string = .data$file,
          pattern = paste0(strace_discards, collapse = "|"),
          negate = TRUE
        ) |
          stringr::str_detect(
            string = .data$file,
            pattern = paste0(strace_keep, collapse = "|")
          )
      )
  } else if (length(strace_discards)) {
    strace_df <- strace_df |>
      dplyr::filter(
        stringr::str_detect(
          string = .data$file,
          pattern = paste0(strace_discards, collapse = "|"),
          negate = TRUE
        )
      )
  }

  # Derive net status (clean duplicate entries)

  strace_df |>
    dplyr::filter(
      .data$type %in% c("read", "write") & !duplicated(strace_df[c("file", "type")]) | # First read or write
        .data$type %in% c("delete") & !duplicated(strace_df[c("file", "type")], fromLast = TRUE) # Last delete
    ) |>
    dplyr::group_by(.data$file) |>
    dplyr::arrange(.data$file, .data$seq) |>
    dplyr::filter(
      .data$type == "read" & !cumsum(.data$type == "write") | # Remove reads from  a file created earlier
        .data$type == "write" & !cumsum(rev(.data$type) == "delete") | # Remove write when the file is deleted afterwards
        .data$type == "delete" & (!cumsum(.data$type == "write") | utils::head(.data$type, 1) == "read") # Remove delete when the file was created earlier, and not read before that creation
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$seq, .data$file) |>
    dplyr::select(.data$time, .data$file, .data$type)
}
