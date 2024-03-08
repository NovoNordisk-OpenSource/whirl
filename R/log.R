#' Run script
#' test with a single script
#'
#' @param script path
#' @param strace logical
#' @param renv logical
#' @param output_dir path
#' @param cleanup logical
#'
#' @export

run_script <- function(script, strace = FALSE, renv = TRUE, cleanup = TRUE, output_dir = NULL) {
  title <- gsub(pattern = "^.*/", replacement = "", x = script)
  script <- normalizePath(script)
  output <- gsub(pattern = "\\.qmd$|\\.R$|\\.Rmd$", replacement = "\\.html", x = script)


  if (is.null(output_dir)) {
    output_dir_val <- dirname(output)
  } else{
    output_dir_val <- (normalizePath(output_dir))
  }

  x <- quarto_render_move(
    input = log_document("log.qmd"),
    execute_params = list(title = title, script = script, strace = strace, renv = renv),
    output_file = basename(output),
    output_dir = output_dir_val
  )

  if (cleanup) {
    list.files(path = dirname(log_document("log.qmd")), pattern = "\\.strace|\\.md", recursive = TRUE, full.names = TRUE) |>
      unlink(recursive = TRUE)
  }

  return(invisible(x))
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

#' `quarto::quarto_render()`, but output file is moved to `output_dir`
#'
#' The default `quarto::quarto_render()` function can only render outputs
#' to the current working directory. This is a wrapper that moves the rendered
#' output to `output_dir`.
#' @param input Path to the input qmd file.
#' @param output_file The name of the output file. If using `NULL` then the
#' output filename will be based on filename for the input file.
#' @param output_dir Path to the output directory.
#' @param ... Other args passed to `quarto::quarto_render()`
#' @export
quarto_render_move <- function(input,
                               output_file = NULL,
                               output_dir = NULL,
                               ...) {
  # https://github.com/jhelvy/jph/blob/master/R/quarto_render_move.R
  # Get all the input / output file names and paths
  x <- quarto::quarto_inspect(input)
  output_format <- names(x$formats)
  output <- x$formats[[output_format]]$pandoc$`output-file`
  if (is.null(output_file)) {
    output_file <- output
  }
  input_dir <- dirname(input)
  if (is.null(output_dir)) {
    output_dir <- input_dir
  }
  output_path_from <- file.path(input_dir, output)
  output_path_to <- file.path(output_dir, output_file)

  # Render qmd file to input_dir
  quarto::quarto_render(input = input, ...)

  # If output_dir is different from input_dir, copy the rendered output
  # there and delete the original file
  if (input_dir != output_dir) {

    # Try to make the folder if it doesn't yet exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }

    # Now move the output to the output_dir and remove the original output
    file.copy(
      from = output_path_from,
      to = output_path_to,
      overwrite = TRUE
    )
    file.remove(output_path_from)

    # If the output_dir is the same as input_dir, but the output_file
    # has a different name from the input file, then just rename it
  } else if (output_file != output) {
    file.rename(from = output_path_from, to = output_path_to)
  }
}

# To avoid NOTEs the R CMD check
utils::globalVariables(".data")
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
