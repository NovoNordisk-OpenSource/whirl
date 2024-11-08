# Getting file extension
get_file_ext <- function(file_paths) {
  sapply(file_paths, function(file_path) {
    file_name <- basename(file_path)
    file_parts <- strsplit(file_name, "\\.")[[1]]
    file_extension <- tail(file_parts, n = 1)
    return(file_extension)
  })
}

# Function to scale a numeric vector to percentage
scale_to_percent <- function(x, digits = 2) {
  percent_values <- x * 100
  formatted_percent_values <- sprintf(paste0("%.", digits, "f%%"), percent_values)
  return(formatted_percent_values)
}

# Function to replace NA values with the last non-NA value
replace_na_with_last <- function(x) {
  last_non_na <- NA
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      last_non_na <- x[i]
    } else if (!is.na(last_non_na)) {
      x[i] <- last_non_na
    }
  }
  return(x)
}

# Function to get relative path
path_rel <- function(path, start = ".") {
  # Normalize the paths
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  start <- normalizePath(start, winslash = "/", mustWork = FALSE)

  # Split the paths into components
  path_parts <- strsplit(path, "/")[[1]]
  start_parts <- strsplit(start, "/")[[1]]

  # Find the common prefix length
  common_length <- 0
  for (i in seq_along(start_parts)) {
    if (i > length(path_parts) || start_parts[i] != path_parts[i]) {
      break
    }
    common_length <- common_length + 1
  }

  # Compute the relative path
  up_levels <- length(start_parts) - common_length
  down_levels <- path_parts[(common_length + 1):length(path_parts)]

  relative_path <- c(rep("..", up_levels), down_levels)

  # Join the components into a single path
  relative_path <- paste(relative_path, collapse = "/")

  return(relative_path)
}
