
# Read strace and rough selection

x <- readLines(strace_log) |>
  stringr::str_squish() |>
  stringr::str_subset("openat|unlink|chdir") |>
  stringr::str_subset(
    pattern = "ENOENT \\(No such file or directory\\)|ENXIO \\(No such device or address\\)| ENOTDIR \\(Not a directory\\)",
    negate = TRUE
    ) |>
  stringr::str_subset("<unfinished \\.{3}>|<\\.{3} [a-zA-Z]+ resumed>", negate = TRUE)

# Need to derive complete path after wd change
# Logic to handle repeated use
# Qualify actions as read, write, delete?

p_wd <- tempdir()

y <- x |>
  unglue::unglue_data(
    patterns = list(
      "{pid} {time} {funct}({keyword}, \"{path}\", {action}, {access}) = {result} <{duration}>",
      "{pid} {time} {funct}({keyword}, \"{path}\", {action}) = {result} <{duration}>",
      "{pid} {time} {funct}(\"{path}\") = {result} <{duration}>"
      )
    ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    pid = as.numeric(pid),
    time = as.POSIXct(as.numeric(time), origin = "1970-01-01"),
    result = as.numeric(result),
    duration = as.numeric(duration),
    type = dplyr::case_when(
      funct == "chdir" ~ "chdir",
      stringr::str_detect(funct, "unlink") ~ "delete",
      stringr::str_detect(action, "O_DIRECTORY") ~ "lookup",
      funct == "openat" & is.na(access) ~ "read",
      funct == "openat" & !is.na(access) ~ "write",
    ),
    dir = dplyr::case_when(
      type == "chdir" & path == "." ~ p_wd,
      type == "chdir" ~ path
      ) |>
      zoo::na.locf(na.rm = FALSE) |>
      dplyr::coalesce(p_wd)
  )

y

# Full paths etc

z <- y |>
  dplyr::transmute(
    time,
    type,
    file = dplyr::if_else(
      stringr::str_detect(string = path, pattern ="^/", negate = TRUE) & keyword == "AT_FDCWD",
      file.path(dir, path),
      path
      )
  ) |>
  dplyr::filter(type %in% c("read", "write", "delete"))

# Remove discards if provided

if (length(options::opt("track_files_discards"))) {
  z <- z |>
    dplyr::filter(
      stringr::str_detect(
        string = file,
        pattern = paste0(options::opt("track_files_discards"), collapse = "|"),
        negate = TRUE)
    )
}

z |> print(n = 100)

# Want to split in read, write, delete?

split(z[c("time", "file")], z$type)

