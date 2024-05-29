#' Add Approved column on session packages tibble
#'
#' A utility function to help you build your approved packages .
#' @noRd
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

#' @noRd
create_approval_plot <- function(data) {
  data |>
    as.data.frame() |>
    count(.data[["Approved"]]) |>
    mutate(pct = prop.table(n),
           status = "Approved",
           lbl = paste0(.data[["Approved"]], ": ", n, "/", sum(n), " (", scales::percent(pct), ")")) |>
    ggplot(aes(x = pct, y = .data[["status"]], fill = .data[["Approved"]], label = lbl)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(position = position_stack(vjust = 0.5, reverse = FALSE)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("Yes" = "green", "No" = "orange")) +
    labs(title = "Approved")

}

