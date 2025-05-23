#' Create biocompute logs
#' 
#' @description
#' BioCompute is a standard for logs of programs for for Bioinformatics Computational Analyses.
#' 
#' The BioCompute object is a `json` log that can be created based on the output of `run()`.
#' 
#' @details
#' The object consists of the following domains:
#'
#' * **Specifications**:
#'   * *spec_version*: Version of BioCompute used (`https://w3id.org/biocompute/1.3.0/``)
#'   * *object_id*: Unique project id
#'   * *type*: Your project type
#'   * *etag*: Your `etag` id from the BioCompute Object Portal
#' 
#' * [Provenance Domain](https://wiki.biocomputeobject.org/index.php?title=Provenance-domain)
#'   * This is used to track the history of the BCO. Review and signatures go here.
#'  
#' * [Usability Domain](https://wiki.biocomputeobject.org/index.php?title=Usability-domain)
#'   * This is used to improve searchability by allowing a free-text description of the BCO.
#'   * Provide external document.
#'  
#' * [Extension Domain](https://wiki.biocomputeobject.org/index.php?title=Extension-domain)
#'   * This is used to add any additional structured information that is not directly covered by the BCO.
#'  
#' * [Description Domain](https://wiki.biocomputeobject.org/index.php?title=Description-domain)
#'   * Contains a structured field for the description of external references, the pipeline steps, and the relationship of I/O objects.
#'   * Provide external document.
#'  
#' * [Execution Domain](https://wiki.biocomputeobject.org/index.php?title=Execution-domain)
#'   * Contains fields for the execution of the BCO.
#'  
#'* [Parametric Domain](https://wiki.biocomputeobject.org/index.php?title=Parametric-domain)
#'   * Represents the list of parameters customizing the computational flow which can affect the output of the calculations.
#'  
#' * [IO Domain](https://wiki.biocomputeobject.org/index.php?title=Iodomain)
#'   * Represents the list of global input and output files created by the computational workflow.
#'  
#' * [Error Domain](https://wiki.biocomputeobject.org/index.php?title=Error-domain)
#'   * Defines the empirical and algorithmic limits and error sources of the BCO.
#'
#' See the [BioCompute Object Portal](https://www.biocomputeobject.org) and the [BioCompute Objects Wiki](https://wiki.biocomputeobject.org) for more information.
#' 
#' @param queue Result from `run()`.
#' @param path A character string specifying the file path to write BioCompute log to.
#' @param ... Additional arguments parsed to `jsonlite::write_json()`. Note always uses `auto_unbox = TRUE`.
#' @return (`invisible`) `list` of the biocompute domains and their content.
#' @export
write_biocompute <- function(
  queue = run("_whirl.yml"), 
  path = "bco.json", 
  ...
) {

  config <- attr(queue, "whirl_input")

  if (is.null(config)) {
    cli::cli_abort("The `queue` must be created with `whirl::run()`")
  } else if (!rlang::is_string(config) || 
    !get_file_ext(config) %in% c("yml", "yaml") ||
    is.null(yaml::read_yaml(config)[["biocompute"]])
    ) {
    cli::cli_abort("Input to `run()` must be a path to a yaml file with a biocompute entry. See `use_biocompute()`.")
  }

  bco <- create_biocompute(queue = queue, config = config)
  
  jsonlite::write_json(x = bco, path = path, auto_unbox = TRUE, ...)

  invisible(bco)
}

#' @noRd
create_biocompute <- function(queue, config) {
  metadata <- yaml::read_yaml(config)

  list(
    object_id = metadata[["biocompute"]][["object_id"]] |>
      get_single_unique(),
    spec_version = metadata[["biocompute"]][["spec_version"]] |>
      get_single_unique(),
    etag = metadata[["biocompute"]][["etag"]] |>
      get_single_unique(),
    provenance_domain = NULL,
    usability_domain = metadata[["biocompute"]][["usability"]] |>
      get_single_unique(),
    extension_domain = metadata[["biocompute"]][["extension"]] |>
      get_single_unique(),
    description_domain = create_description_domain(queue),
    execution_domain = create_execution_domain(queue),
    parametric_domain = create_parametrics_domain(metadata, dirname(config)),
    io_domain = create_io_domain(queue),
    error_domain = list(
      algorithmic_error = NULL,
      empirical_error = NULL
    ) # TODO
  )
}

# DESCRIPTION DOMAIN
#' @noRd
create_description_domain <- function(queue) {

  pipeline_steps <- vector(mode = "list", length = length(queue$id))
  for (step in seq_along(pipeline_steps)) {
    pipeline_steps[[step]]$name <- basename(queue$script[[step]]) |>
      sub(pattern = "\\.\\w+$", replacement = "") |>
      gsub(pattern = "[-_]", replacement = " ")
    pipeline_steps[[step]]$step_number <- queue$id[[step]]
    pipeline_steps[[step]]$version <- NULL #TODO
    pipeline_steps[[step]]$description <- NULL # TODO - consider taking from header?


    pipeline_steps[[step]]$prerequisite <- queue$result[[step]]$session_info_rlist$environment_options.packages |>
      tibble::as_tibble() |> 
      dplyr::mutate(
        name = paste("R package:", .data$package, "- version:", .data$ondiskversion),
        uri = lapply(.data$package, function(x) utils::packageDescription(x)$URL)
      ) |>
      dplyr::select("name", "uri") |>
      purrr::pmap(.f = list)

    if (is.null(queue$result[[step]]$session_info_rlist$log_info.read)) {
      pipeline_steps[[step]]$input_list <- list()
    } else {
      pipeline_steps[[step]]$input_list <- queue$result[[step]]$session_info_rlist$log_info.read |>
        dplyr::mutate(
          filename = basename(file),
          time = format(.data$time, format = "%Y-%m-%d %H:%M:%S %Z")
        ) |>
        dplyr::rename(uri = "file", access_time = "time") |>
        dplyr::select("filename", "uri", "access_time") |>
        purrr::pmap(.f = list)
    }

    if (is.null(queue$result[[step]]$session_info_rlist$log_info.write)) {
      pipeline_steps[[step]]$output_list <- list()
    } else {
      pipeline_steps[[step]]$output_list <- queue$result[[step]]$session_info_rlist$log_info.write |>
        dplyr::mutate(
          filename = basename(.data$file),
          time = format(.data$time, format = "%Y-%m-%d %H:%M:%S %Z")
        ) |>
        dplyr::rename(uri = "file", access_time = "time") |>
        dplyr::select("filename", "uri", "access_time") |>
        purrr::pmap(.f = list)
    }
  }

  description_domain <- list(
    keywords = list(), # TODO
    External_Reference = list(), #TODO
    pipeline_steps = pipeline_steps
  )

  return(description_domain)
}

#' @noRd
get_single_unique <- function(x) {
  x <- x |>
    unlist() |>
    unique()

  stopifnot(length(x) == 1)

  return(x)
}

#' @noRd
get_unique_values <- function(x) {
  split(x, names(x)) |>
    lapply(\(x) x |> unlist() |> unique() |> paste(collapse = ";"))
}

#' @noRd
create_execution_domain <- function(queue) {

  envvars <- queue$result |>
    purrr::map(c("session_info_rlist", "environment_options.environment")) |> 
    purrr::list_rbind()

  software_prerequisites <- list(
    list(
      name = "R",
      version = sub(
        pattern = "R version ([0-9]+\\.[0-9]+\\.[0-9]+).*", 
        replacement = "\\1", 
        x = queue$result |>
          purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
          get_single_unique()
      ),
      URI = "https://www.r-project.org/"
    ),
    list(
      name = "quarto",
      version = queue$result |>
        purrr::map(c("session_info_rlist", "environment_options.platform", "quarto")) |>
        get_single_unique(),
      URI = "https://quarto.org"
    ),
    list(
      name = "pandoc",
      version = queue$result |>
        purrr::map(c("session_info_rlist", "environment_options.platform", "pandoc")) |>
        get_single_unique(),
      URI = "https://pandoc.org/"
    )
  )

  packages <- queue$result |>
    purrr::map(c("session_info_rlist", "environment_options.packages")) |>
    purrr::map(~ tibble::tibble(
      name = .x$package,
      version = .x$loadedversion,
      uri = sapply(.x$package, function(x) utils::packageDescription(x)$URL)
    )) |>
    purrr::list_rbind() |> 
    purrr::pmap(.f = list)

  software_prerequisites <- append(software_prerequisites, packages)

  execution_domain <- list(
    script = queue$script,
    script_driver = queue$result |>
      purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
      get_single_unique(),
    software_prerequisites = software_prerequisites,
    external_data_endpoints = list(), # TODO
    environment_variables = stats::setNames(envvars$Value, envvars$Setting) |>
      as.list() |>
      get_unique_values()
  )

  return(execution_domain)
}

#' @noRd
create_parametrics_domain <- function(config, base_path) {
  parametric_domain = list()
  step_number = 0
  for (step in config$steps) {
    if (!("parameter_files" %in% names(step))) {
      step_number = step_number + 1
      next
    }

    for (parameter_file in step$parameter_files) {
      parameters <- yaml::read_yaml(normalize_with_base(parameter_file, base_path))

      parametric_domain <- append(
        parametric_domain,
        purrr::map2(
          parameters,
          names(parameters),
          \(x,y) list(param = y, value = x, step = step_number)
        ) |>
          unname()
      )
    }
    step_number = step_number + 1
  }
  return(parametric_domain)
}


#' @noRd
bco_create_outputs <- function(files) {

  entry <- vector(mode = "list", length = length(files))
  for (i in seq_along(entry)) {

    entry[[i]]$mediatype <- if (grepl("\\.html$", files[[i]])) {
      "text/html"
    } else if (grepl("\\.zip$", files[[i]])) {
      "application/zip"
    } else if (grepl("\\.csv$", files[[i]])) {
      "text/csv"
    } else if (grepl("\\.txt$", files[[i]])) {
      "text/txt"
    } else {
      " "
    }

    entry[[i]]$uri <- list(
      filename = basename(files[[i]]),
      uri = files[[i]]
    )
  }

  return(entry)
}

#' @noRd
bco_create_inputs <- function(files) {

  entry <- vector(mode = "list", length = length(files))
  for (i in seq_along(entry)) {

    entry[[i]]$uri <- list(
      uri = files[[i]]
    )
  }

  return(entry)
}

#' @noRd
create_io_domain <- function(queue) {
  input <- queue$result |>
    purrr::map(c("session_info_rlist", "log_info.read", "file")) |>
    unlist() |>
    unique() |>
    bco_create_inputs()

  output <- queue$result |>
    purrr::map(c("session_info_rlist", "log_info.write", "file")) |>
    unlist() |>
    unique() |>
    bco_create_outputs()


  return(list(
    input_subdomain = input,
    output_subdomain = output
  ))
}
