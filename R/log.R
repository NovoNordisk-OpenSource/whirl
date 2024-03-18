#' Run script
#' test with a single script
#'
#' @param script path
#' @param track_files logical
#' @param renv logical
#' @param out_dir description
#' @export

run_script <- function(script, track_files = FALSE, renv = TRUE, out_dir = dirname(script)) {

  stopifnot(file.exists(script))
  stopifnot(tools::file_ext(script) %in% c("R", "qmd", "Rmd"))
  stopifnot(is.logical(track_files))
  stopifnot(is.logical(renv))
  stopifnot(dir.exists(out_dir))

  # Create temp files for all documents.
  # Note: Documents are copied from package folder to make sure nothing is evaluated there.

  dummy_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/dummy.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  log_qmd <- withr::local_tempfile(
    lines = readLines(system.file("documents/log.qmd", package = "whirl")),
    fileext = ".qmd"
    )

  doc_md <- withr::local_tempfile(fileext = ".md")

  # TODO: Temp path to store strace

  log_html <- withr::local_tempfile(fileext = ".html")

  p <- callr::r_session$new()

  p$run(
    func = setwd,
    args = list(dir = tempdir())
  )

  p$run(getwd)

  # TODO: Start strace

  p$run(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = dummy_qmd,
      output_format = "markdown",
      output_file = basename(doc_md),
      execute_params = list(script = normalizePath(script)),
      execute_dir = getwd()
    )
  )

  # TODO: Stop strace?
  # TODO: Strace input path

  p$run(
    func = \(...) quarto::quarto_render(...),
    args = list(
      input = log_qmd,
      output_file = basename(log_html),
      execute_params = list(title = script, script_md = doc_md),
      execute_dir = getwd()
    )
  )

  p$close()

  output <- file.path(out_dir, gsub(pattern = "\\.[^\\.]*$", replacement = ".html", x = basename(script)))

  file.copy(
    from = log_html,
    to = output
  )

  return(invisible(output))
}

#' Internal log documents
#' @param doc name
#' @export

log_document <- function(doc) {
  system.file("documents", doc, package = "whirl")
}

#' Example scripts
#' @param doc name
#' @export

log_example <- function(doc) {
  system.file("examples", doc, package = "whirl")
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

read_strace <- function(path, strace_discards = c("^/lib", "^/etc", "^/lib64", "^/usr", "^/var", "^/opt", "^/sys", "^/proc", "^/tmp", "^.$", paste0("^", .libPaths()))) {
  all_strace <- readLines(path)

  strace_filter <- grep("openat\\(AT_FDCWD|unlink\\(|chdir\\(", all_strace, value = TRUE)
  strace_filter <- grep("ENOENT \\(No such file or directory\\)|ENXIO \\(No such device or address\\)| ENOTDIR \\(Not a directory\\)", strace_filter, value = TRUE, invert = TRUE)
  # data_strace <- tidyr::separate(data.frame(x = strace_filter), .data$x, sep = '(\\sopenat\\(AT_FDCWD,\\s\\")|(unlink\\(\\")|(chdir\\(\\")',  into = c("time", "file"), fill = "right", remove = FALSE) %>%
  data_strace <- tidyr::separate(data.frame(x = strace_filter), .data$x, sep = '[^,]\\s|\\([a-zA-Z_,\\s]*\\"', into = c("time", "type", "file"), fill = "right", extra = "merge", remove = TRUE) %>%
    tidyr::separate(.data$file, sep = "\\)\\s*= ", into = c("file", "num"), remove = FALSE) %>%
    tidyr::separate(.data$file, sep = '\\", ', into = c("file", "what"), remove = FALSE, fill = "right") %>%
    tidyr::separate(.data$what, sep = ", ", into = c("what", "access"), remove = FALSE, fill = "right") %>%
    tidyr::separate(.data$num, sep = " <", into = c("num", "duration"), remove = FALSE, fill = "right")

  data_strace$entrynum <- seq_len(nrow(data_strace))
  data_strace$file <- gsub('\\"', "", data_strace$file)

  relative_files <- data_strace[!grepl("^/", data_strace$file) & data_strace$type != "chdir", "entrynum"]
  chdirs <- c(0, data_strace[grepl("^/", data_strace$file) & data_strace$type == "chdir", "entrynum"])
  rel_chdirs <- c(0, data_strace[!grepl("^/", data_strace$file) & data_strace$type == "chdir", "entrynum"])

  max.ch <- 0
  rel_dirs <- list()
  file_paths <- vector()
  for (i in seq_along(relative_files)) {
    pos <- chdirs[chdirs < relative_files[i]]
    max.ch[i] <- pos[length(pos)]
    rel_dirs[[i]] <- rel_chdirs[max.ch[i] < rel_chdirs & rel_chdirs < relative_files[i]]
    chdirs <- chdirs[chdirs >= max.ch[i]]

    if (max.ch[i] == 0) {
      file_paths[i] <- paste(c(getwd(), data_strace$file[c(rel_dirs[[i]], relative_files[i])]), collapse = "/")
    } else {
      file_paths[i] <- paste(data_strace$file[c(max.ch[i], rel_dirs[[i]], relative_files[i])], collapse = "/")
    }
  }

  data_strace$file[relative_files] <- file_paths


  data_strace <- data_strace[grep(paste(strace_discards, collapse = "|"), data_strace$file, invert = TRUE), ]

  if (nrow(data_strace)) {
    data_strace$time <- as.POSIXct(as.numeric(data_strace$time), origin = "1970-01-01")
    data_strace$duration <- as.numeric(gsub(">", "", data_strace$duration))
    data_strace$action <- NA
    data_strace$action[data_strace$type == "chdir"] <- "Change dir"
    data_strace$action[is.na(data_strace$what) & data_strace$num == 0 & data_strace$type == "unlink"] <- "Deleted"
    data_strace$action[is.na(data_strace$action) & grepl("O_DIRECTORY", data_strace$what)] <- "Lookup"
    data_strace$action[is.na(data_strace$action) & is.na(data_strace$access)] <- "Read"
    data_strace$action[is.na(data_strace$action) & !is.na(data_strace$access)] <- "Write"
  }

  data_strace$file <- gsub(normalizePath("~"), "~", data_strace$file)

  return(data_strace[data_strace$type != "chdir", ])
}

#' refine strace output
#'
#' @param data_strace - file lines
#'
#' @return tibble
#' @export
refine_strace <- function(data_strace) {
  # remove consecutive duplicates

  rm_dup <- rle(paste(data_strace$file, data_strace$num, data_strace$action))
  data_strace_s1 <- data_strace[cumsum(c(1, rm_dup$lengths[-length(rm_dup$lengths)])), ]

  # First entry
  data_strace_s1_first <- data_strace_s1[!duplicated(data_strace_s1$file), ]

  # last entry
  data_strace_s1_last <- data_strace_s1[!duplicated(data_strace_s1$file, fromLast = TRUE), ]

  # Input files: if first entry is read
  input_files <- data_strace_s1_first$file[data_strace_s1_first$action == "Read"]

  # Temporary: if write followed by deleted
  writes <- data_strace_s1$file[data_strace_s1$action == "Write"]
  last_deleted <- data_strace_s1_last$file[data_strace_s1_last$action == "Deleted"]

  temporary_files <- setdiff(intersect(writes, last_deleted), input_files)

  # Output:
  output_files <- setdiff(writes, temporary_files)

  # Deleted
  deleted_files_all <- data_strace_s1$file[data_strace_s1$action == "Deleted"]
  deleted_files <- setdiff(deleted_files_all, c(input_files, temporary_files, output_files))

  input <- data_strace[data_strace$file %in% input_files, ]
  output <-  data_strace[data_strace$file %in% output_files, ]
  temporary <- data_strace[data_strace$file %in% temporary_files, ]
  deleted <- data_strace[data_strace$file %in% deleted_files, ]

  input <- input[!duplicated(input$file), ]
  output <- output[!duplicated(output$file), ]
  temporary <- temporary[!duplicated(temporary$file), ]
  deleted <- deleted[!duplicated(deleted$file), ]

  list(input = input,
       output =  output,
       temporary =  temporary,
       deleted =  deleted)

}
