#' Check if only using approved packages
#' Both used and approved packages to be specified
#' as: {pkgname}${pkgversion}
#' @noRd
check_approved <- function(used, approved = NULL) {
  if (is.null(approved)) {
    return(NA)
  }

  used <- package_spec(used)
  names_used <- names(used)
  approved <- package_spec(approved)

  ok <- logical(length = length(used))

  for (i in which(names_used %in% names(approved))) {
    ok[[i]] <- used[[i]] == approved[[names_used[[i]]]]
  }

  ok
}

#' @noRd
package_spec <- function(x) {
  if (!length(x)) {
    return(character())
  }

  x <- strsplit(x = x, split = "@")
  n <- vapply(X = x, FUN = \(x) x[[1]], FUN.VALUE = character(1))
  v <- vapply(X = x, FUN = \(x) x[[2]], FUN.VALUE = character(1))
  stats::setNames(object = v, nm = n)
}
