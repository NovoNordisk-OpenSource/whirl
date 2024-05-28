#' Add Approved column on session packages tibble
#'
#' A utility function to help you build your approved packages .
#'
#' @param approved_pkg_loc The library location where the approved packages are
#' @param session_pkgs The loaded packages in the current session
#' @param output_file String. Name of file where the modified tibble will be saved.
#'
#' @return session_pkgs with additional column of Approved indicating if the package is approved or not
#' @export
#'
#' @examples
#' approved_pkgs <- c( "/opt/R/4.2.0/lib/R/library", "/opt/R/4.2.0/lib/R/site-library" )
#' # build and return
#' check_approved(approved_pkgs, sessioninfo::package_info())
#'
#' # build and save
#' dir <- tempdir()
#' check_approved(approved_pkgs, sessioninfo::package_info(), file.path(dir, "approved.rds"))
#'
check_approved <- function(approved_pkg_loc,
                           session_pkgs,
                           output_file = NULL) {
  if (is.null(approved_pkg_loc)) {
    stop("approved_pkg_loc cannot be NULL")
  }
  approved_dset <- session_pkgs |>
    # as.data.frame() |>
    dplyr::mutate(Approved = ifelse(library %in% approved_pkg_loc, "Yes", "No"))

  if (is.null(output_file)) {
    approved_dset
  } else {
    saveRDS(approved_dset, output_file)
  }
}
