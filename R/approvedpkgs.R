#' Add Approved column on session packages tibble
#'
#' An utility function to help you build your approved packages .
#' @noRd
# nolint start
check_approved <- function(
  approved_pkg_folder,
  approved_pkg_url,
  session_pkgs,
  output_file = NULL
) {
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

      status <- check_url(tmpf)

      if (!status) {
        stop("The repository is not available")
      }

      src_url <- utils::available.packages(url) |>
        as.data.frame() |>
        dplyr::mutate(
          Repository = url
        ) |>
        dplyr::select("Package", "Version", "Repository")
      session_pkgs |>
        dplyr::left_join(
          y = src_url,
          by = c("package" = "Package", "loadedversion" = "Version")
        ) |>
        dplyr::mutate(
          Approved = ifelse(is.na(.data[["Repository"]]), "No", "Yes"),
          "Approved Repository" = url
        ) |>
        dplyr::arrange(.data[["Approved"]], .data[["package"]]) |>
        dplyr::select(-c("Repository"))
    })
    approved_dset_url <- do.call(dplyr::bind_rows, approved_dset_url_list)
  }

  if (!is.null(approved_pkg_folder) && length(approved_pkg_folder) > 0) {
    approved_dset_file_list <- lapply(approved_pkg_folder, function(folder) {
      if (!dir.exists(folder)) {
        stop("The folder does not exist")
      }
      src_file <- installed_packages(folder)

      session_pkgs |>
        dplyr::left_join(
          y = src_file,
          by = c("package" = "Package", "loadedversion" = "Version")
        ) |>
        dplyr::mutate(
          Approved = ifelse(is.na(.data[["Repository"]]), "No", "Yes"),
          "Approved Repository" = folder
        ) |>
        dplyr::arrange(.data[["Approved"]], .data[["package"]]) |>
        dplyr::select(-c("Repository"))
    })
    approved_dset_file <- do.call(dplyr::bind_rows, approved_dset_file_list)
  }

  if (is.null(approved_pkg_folder)) {
    approved_dset <- approved_dset_url |>
      dplyr::select(
        "package",
        "loadedversion",
        "date",
        "source",
        "Approved",
        "Approved Repository"
      ) |>
      dplyr::rename("Repository URL" = "Approved Repository") |>
      dplyr::arrange(.data[["Approved"]], .data[["package"]])
  } else if (is.null(approved_pkg_url) || length(approved_pkg_url) == 0) {
    approved_dset <- approved_dset_file |>
      dplyr::select(
        "package",
        "loadedversion",
        "date",
        "source",
        "Approved",
        "Approved Repository"
      ) |>
      dplyr::rename("Repository Folder" = "Approved Repository") |>
      dplyr::arrange(.data[["Approved"]], .data[["package"]])
  } else {
    approved_dset <- dplyr::full_join(
      x = approved_dset_url,
      y = approved_dset_file,
      by = c("package", "loadedversion")
    ) |>
      dplyr::select(
        "package",
        "loadedversion",
        "date.x",
        "source.x",
        "Approved.x",
        "Approved Repository.x",
        "Approved.y",
        "Approved Repository.y"
      ) |>
      dplyr::rename(
        "date" = "date.x",
        "source" = "source.x",
        "Repository URL" = "Approved Repository.x",
        "Repository Folder" = "Approved Repository.y",
        "Approved in Repository URL" = "Approved.x",
        "Approved in Repository Folder" = "Approved.y"
      ) |>
      dplyr::arrange(
        .data[["Approved in Repository URL"]],
        .data[["Approved in Repository Folder"]],
        .data[["package"]]
      )
  }

  if (is.null(output_file)) {
    approved_dset
  } else {
    saveRDS(approved_dset, output_file)
  }
}
# nolint end

#' Retrieve installed packages and their version
#' Idea is that this helper function will only be called using an installation
#' folder with a limited number of approved packages present, and this way only
#' the DESCRIPTION file is read from each package unlike installed.packages.
#' @noRd
installed_packages <- function(folder) {
  x <- data.frame(
    Package = sort(list.files(path = folder)),
    Version = NA_character_,
    Repository = folder
  )

  x[["Version"]] <- vapply(
    X = x[["Package"]],
    FUN = function(x) {
      tryCatch(
        utils::packageDescription(pkg = x, lib.loc = folder, fields = "Version"),
        warning = \(w) NA_character_,
        error = \(e) NA_character_
      )
    }
    ,
    FUN.VALUE = character(1)
  )

  x[!is.na(x[["Version"]]), ]
}


#' @noRd
create_approval_plot <- function(data) {
  rlang::check_installed("ggplot2")
  row.names(data) <- NULL

  data$grpvar <- ifelse(
    rowSums(as.matrix(data[, grepl("^Approved", colnames(data))]) == "No") ==
      ncol(as.matrix(data[, grepl("^Approved", colnames(data))])),
    "No",
    "Yes"
  )

  data |>
    dplyr::count(.data[["grpvar"]]) |>
    dplyr::mutate(
      pct = prop.table(.data[["n"]]),
      status = "grpvar",
      lbl = paste0(
        .data[["grpvar"]],
        ": ",
        .data[["n"]],
        "/",
        sum(.data[["n"]]),
        " (",
        scale_to_percent(.data[["pct"]]),
        ")"
      )
    ) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data[["pct"]],
        y = .data[["status"]],
        fill = .data[["grpvar"]],
        label = .data[["lbl"]]
      )
    ) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::geom_text(
      position = ggplot2::position_stack(vjust = 0.5, reverse = FALSE)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_fill_manual(values = c("Yes" = "green", "No" = "orange")) +
    ggplot2::labs(title = "Approved")
}

#' @noRd
check_url <- function(url) {
  status <- tryCatch(
    {
      con <- url(url, "r")
      readLines(con, n = 1)
      close(con)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  return(status)
}
