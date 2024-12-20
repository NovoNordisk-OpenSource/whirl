#' Normalize a Path with Respect to a Base Directory
#'
#' This function normalizes a given path with respect to a specified base
#' directory.
#' If the path is relative, it combines the base directory and the path, then
#' normalizes the resulting path.
#' If the path is absolute or starts with `~`, it normalizes the path directly.
#' If no base directory is specified, the current working directory is used as
#' the base.
#'
#' @param path A character string representing the path to be normalized.
#' Can be relative, absolute, or start with `~`.
#' @param base A character string representing the base directory with respect
#' to which the path should be normalized. The default is the current working
#' directory (".").
#' @return A character string representing the normalized path.
#' @examples
#' \dontrun{
#' base <- "/my/base/directory"
#' relative_path <- "subdir/file.txt"
#' normalized_path <- normalize_with_base(relative_path, base)
#' print(normalized_path)
#'
#' # Using the current working directory as the base
#' relative_path <- "subdir/file.txt"
#' normalized_path <- normalize_with_base(relative_path)
#' print(normalized_path)
#'
#' # Using an absolute path
#' absolute_path <- "/another/directory/subdir/file.txt"
#' normalized_path <- normalize_with_base(absolute_path)
#' print(normalized_path)
#'
#' # Using a path starting with ~
#' home_path <- "~/subdir/file.txt"
#' normalized_path <- normalize_with_base(home_path)
#' print(normalized_path)
#' }
#' @noRd
normalize_with_base <- function(path, base = ".") {
  # Expand any ~ in the path to the full home directory path
  path <- path.expand(path)

  # Check if the path is absolute
  if (!grepl("^(/|[a-zA-Z]:)", path)) {
    # Combine the base and the relative path if the path is not absolute
    path <- file.path(base, path)
  }

  # Normalize the path
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  return(normalized_path)
}
