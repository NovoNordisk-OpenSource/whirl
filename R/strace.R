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

read_strace_info <- function(path, p_wd, strace_discards = character(), types = c("read", "write", "deleted")) {
  strace <- path |>
    read_strace(p_wd = p_wd) |>
    refine_strace(strace_discards = strace_discards)

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
      pid = as.numeric(pid),
      time = as.POSIXct(as.numeric(time), origin = "1970-01-01"),
      result = as.numeric(result),
      duration = as.numeric(duration),
      type = dplyr::case_when(
        funct == "chdir" ~ "chdir",
        stringr::str_detect(funct, "unlink") ~ "delete",
        funct == "openat" & stringr::str_detect(action, "O_DIRECTORY") ~ "lookup",
        funct == "openat" & is.na(access) ~ "read",
        funct == "openat" & !is.na(access) ~ "write",
      ),
      wd = dplyr::case_when(
        type == "chdir" & path == "." ~ p_wd,
        type == "chdir" ~ path
      ) |>
        zoo::na.locf(na.rm = FALSE) |>
        dplyr::coalesce(p_wd),
      dir = dplyr::coalesce(dir, wd)
    )

  # Full paths etc

  strace_df |>
    dplyr::mutate(
      time,
      type,
      file = dplyr::if_else(
        stringr::str_detect(string = path, pattern = "^/", negate = TRUE),
        file.path(dir, path),
        path
      )
    ) |>
    dplyr::filter(type %in% c("read", "write", "delete")) |>
    dplyr::select(seq, time, file, type)
}

#' refine strace output
#'
#' @param strace_df [data.frame] with strace information as returned from `read_strace`
#' @param strace_discards [character] keywords to use to discard files from the info
#' @return [data.frame] with strace information where discarded and duplicate files are removed
#' @noRd

refine_strace <- function(strace_df, strace_discards = character()) {
  # Remove discards if provided

  if (length(strace_discards)) {
    strace_df <- strace_df |>
      dplyr::filter(
        stringr::str_detect(
          string = file,
          pattern = paste0(strace_discards, collapse = "|"),
          negate = TRUE
        )
      )
  }

  # Derive net status (clean duplicate entries)

  strace_df |>
    dplyr::filter(
      type %in% c("read", "write") & !duplicated(strace_df[c("file", "type")]) | # First read or write
        type %in% c("delete") & !duplicated(strace_df[c("file", "type")], fromLast = TRUE) # Last delete
    ) |>
    dplyr::group_by(file) |>
    dplyr::arrange(file, seq) |>
    dplyr::filter(
      type == "read" & !cumsum(type == "write") | # Remove reads from  a file created earlier
        type == "write" & !cumsum(rev(type) == "delete") | # Remove write when the file is deleted afterwards
        type == "delete" & (!cumsum(type == "write") | head(type, 1) == "read") # Remove delete when the file was created earlier, and not read before that creation
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(seq, file) |>
    dplyr::select(time, file, type)
}
