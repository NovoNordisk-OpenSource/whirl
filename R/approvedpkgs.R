
#' Add Approved column on session packages tibble
#'
#' A utility function to help you build your approved packages .
#' @noRd
check_approved <- function(approved_pkg_folder,
                           approved_pkg_url,
                           session_pkgs,
                           output_file = NULL) {
  if (is.null(approved_pkg_folder) && is.null(approved_pkg_url)) {
    stop("Both approved_pkg_folder and approved_pkg_url cannot be NULL")
  }

  if (!is.null(approved_pkg_url) && length(approved_pkg_url) > 0) {
    approved_dset_url_list <- lapply(approved_pkg_url, function(url) {
      if (!endsWith(url, "src/contrib")) {
        url <- paste0(url, "/src/contrib")
      }
      tmpf <- if (startsWith(url, "file:///")) {
        tmpf <- url
        if (.Platform$OS.type == "windows" && grepl("^/[A-Za-z]:", tmpf)) {
          tmpf <- substr(tmpf, 2L, nchar(tmpf))
        }
      } else {
        paste0(url, "/PACKAGES")
      }
      if (httr::http_error(tmpf)) {
        stop("The repository is not available")
      }

      src_url <- available.packages(url) |> as.data.frame() |>
        dplyr::mutate(
          Repository = url
        ) |>
        dplyr::select("Package", "Version", "Repository")
      session_pkgs |>
        dplyr::left_join(src_url, by = c("package" = "Package", "loadedversion" = "Version")) |>
        dplyr::mutate(
          Approved = ifelse(is.na(Repository), "No", "Yes"),
          "Approved Repository" = url
        ) |>
        dplyr::arrange(Approved, package) |>
        dplyr::select(-c("Repository"))
    })
    approved_dset_url <- do.call(dplyr::bind_rows, approved_dset_url_list)
  }

  if (!is.null(approved_pkg_folder) && length(approved_pkg_folder) > 0) {
    approved_dset_file_list <- lapply(approved_pkg_folder, function(folder) {
      session_pkgs |>
        dplyr::mutate(
          Approved = ifelse(library %in% folder, "Yes", "No"),
          "Approved Repository" = folder
        ) |>
        dplyr::arrange(Approved, package)
    })
    approved_dset_file <- do.call(dplyr::bind_rows, approved_dset_file_list)
  }

  if (is.null(approved_pkg_folder)) {
    approved_dset <- approved_dset_url |>
      dplyr::select("package", "loadedversion", "date", "source", "Approved", "Approved Repository") |>
      dplyr::rename("Repository URL" = "Approved Repository") |>
      dplyr::arrange(Approved, package)
  } else if (is.null(approved_pkg_url) || length(approved_pkg_url) == 0) {
    approved_dset <- approved_dset_file |>
      dplyr::select("package", "loadedversion", "date", "source", "Approved", "Approved Repository") |>
      dplyr::rename("Repository Folder" = "Approved Repository") |>
      dplyr::arrange(Approved, package)
  } else {
    approved_dset <- dplyr::full_join(approved_dset_url, approved_dset_file, by = c("package", "loadedversion")) |>
      dplyr::select(package, loadedversion, date.x, source.x, Approved.x, Approved.y, "Approved Repository.x", "Approved Repository.y") |>
      dplyr::rename(
        "date" = "date.x",
        "source" = "source.x",
        "Repository URL" = "Approved Repository.x",
        "Repository Folder" = "Approved Repository.y",
        "Approved in Repository URL" = "Approved.x",
        "Approved in Repository Folder" = "Approved.y"
      ) |>
      dplyr::arrange(`Approved in Repository URL`, `Approved in Repository Folder`, package)
  }

  if (is.null(output_file)) {
    approved_dset
  } else {
    saveRDS(approved_dset, output_file)
  }
}


#' @noRd
create_approval_plot <- function(data) {
  library(ggplot2)
  library(dplyr)

  row.names(data) <- NULL

  data$grpvar <- ifelse(rowSums(as.matrix(data[, grepl("^Approved", colnames(data))]) == "No") == ncol(as.matrix(data[, grepl("^Approved", colnames(data))])), "No", "Yes")

  data |>
    count(.data[["grpvar"]]) |>
    mutate(
      pct = prop.table(n),
      status = "grpvar",
      lbl = paste0(.data[["grpvar"]], ": ", n, "/", sum(n), " (", scales::percent(pct), ")")
    ) |>
    ggplot(aes(x = pct, y = .data[["status"]], fill = .data[["grpvar"]], label = lbl)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(position = position_stack(vjust = 0.5, reverse = FALSE)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_fill_manual(values = c( "Yes" = "green", "No" = "orange")) +
    labs(title = "Approved")
}
