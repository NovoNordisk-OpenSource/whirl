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
