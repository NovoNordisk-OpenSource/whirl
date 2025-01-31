# "spec_version" : "https://w3id.org/biocompute/1.3.0/",
# "object_id": "https://example.com/bco/9487ae7e-c1aa-4a3c-b18f-3d3695b33ace",
# "type": "antiviral_resistance_detection",
# "etag": "584C7FE128717E1712426AB19CAAEA8BC1E27365B54285BBEA1221284C7D3A48",
#
# "provenance_domain": {
#           - This is used to track history of the BCO. review and signatures go here.
# },
# "usability_domain": [
#           - This is used to improve searchability by allowing a free-text description of the BCO
#           - Provide external document
# ],
# "extension_domain":{
#           - This is used to add any additional structured information that is not directly covered by the BCO
# },
# "description_domain": {
#           - contains a structured field for the description of external references, the pipeline steps, and the relationship of I/O objects.
#           - Provide external document
# },
# "execution_domain": {
#           - contains fields for the execution of the BCO
# },
# "parametric_domain": {
#           - represents the list of parameters customizing the computational flow which can affect the output of the calculations
# },
# "io_domain": {
#           - represents the list of global input and output files created by the computational workflow
# },
# "error_domain": {
#           - defines the empirical and algorithmic limits and error sources of the BCO
# }

#' @export
create_biocompute <- function(queue, config) {
  metadata <- yaml::read_yaml(config)

  list(
    object_id = metadata[["biocompute"]][["object_id"]] |>
      get_single_unique(),
    spec_version = metadata[["biocompute"]][["spec_version"]] |>
      get_single_unique(),
    etag = metadata[["biocompute"]][["etag"]] |>
      get_single_unique(),
    provenance_domain = NULL, # TODO
    usability_domain = metadata[["biocompute"]][["usability"]] |>
      get_single_unique(),
    extension_domain = list(), #TODO
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

#' @export
write_biocompute <- function(bco, path = "bco.json", auto_unbox = TRUE, ...) {
  jsonlite::write_json(x = bco, path = path, auto_unbox = TRUE, ...)
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
    pipeline_steps[[step]]$version <- NULL
    pipeline_steps[[step]]$description <- NULL
    pipeline_steps[[step]]$input_list <- queue$result[[step]]$session_info_rlist$log_info.read |>
      dplyr::mutate(
        filename = basename(file),
        time = format(time, format = "%Y-%m-%d %H:%M:%S %Z")
      ) |>
      dplyr::rename(uri = file, access_time = time) |>
      dplyr::select(filename, uri, access_time) |>
      purrr::transpose()
  }

  description_domain <- list(
    keywords = list(), # TODO
    External_Reference = list(), #TODO
    pipeline_steps = pipeline_steps
  )

  return (description_domain)
}


# EXECUTION DOMAIN
# Here we should be more dynamic, this setup is more made to fit Bifrost.
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
    purrr::map_dfr(c("session_info_rlist", "environment_options.environment"))

  software_prerequisites <- list(
    list(
      name = "R",
      version = sub("R version ([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", queue$result |>
                      purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
                      get_single_unique()),
      URI = "https://www.r-project.org/"
    ),
    list(
      name = "quarto",
      version = queue$result |>
        purrr::map(c("session_info_rlist", "environment_options.platform", "quarto")) |>
        get_single_unique(),
      URI = "https://cran.r-project.org/web/packages/quarto/index.html"
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
    purrr::map_dfr(~ list(
      name = .x$package,
      version = .x$loadedversion,
      uri = lapply(.x$package, function(x) packageDescription(x)$URL)
    )) |>
    purrr::transpose() |>
    unique()

  software_prerequisites <- append(software_prerequisites, packages)

  execution_domain <- list(
    script = queue$script,
    script_driver = queue$result |>
      purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
      get_single_unique(),
    software_prerequisites = software_prerequisites,
    external_data_endpoints = list(), # TODO
    environment_variables = setNames(envvars$Value, envvars$Setting) |>
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
