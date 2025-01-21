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
#           - Provide external document
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

bco_create_inputs <- function(files) {

  entry <- vector(mode = "list", length = length(files))
  for (i in seq_along(entry)) {

    entry[[i]]$uri <- list(
      uri = files[[i]]
    )
  }

  return(entry)
}

# IO DOMAIN
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

# EXECUTION DOMAIN
# Here we should be more dynamic, this setup is more made to fit Bifrost.

get_single_unique <- function(x) {
  x <- x |>
    unlist() |>
    unique()

  stopifnot(length(x) == 1)

  return(x)
}

create_execution_domain <- function(queue) {

  envvars <- queue$result |>
    purrr::map_dfr(c("session_info_rlist", "environment_options.environment"))

  execution_domain <- list(
    script = queue$script,
    script_driver = queue$result |>
      purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
      get_single_unique(),
    software_prerequisites = list(
      list(
        name = "R",
        version = sub("R version ([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", queue$result |>
                        purrr::map(c("session_info_rlist", "environment_options.platform", "version")) |>
                        get_single_unique())
      ),
      list(
        name = "quarto",
        version = queue$result |>
          purrr::map(c("session_info_rlist", "environment_options.platform", "quarto")) |>
          get_single_unique()
      ),
      list(
        name = "pandoc",
        version = queue$result |>
          purrr::map(c("session_info_rlist", "environment_options.platform", "pandoc")) |>
          get_single_unique()
      )
    ),
    external_data_endpoints = list(),
    environment_variables = setNames(envvars$Value, envvars$Setting) |>
      as.list()
  )

  return(execution_domain)
}


# PARAMETRIC DOMAIN
create_parametrics_domain <- function(execution_overview) {
  execution_info = yaml::read_yaml(execution_overview)

  parametric_domain = list()
  step_number = 0
  for (step in execution_info$steps) {
    if (!("parameter_files" %in% names(step))) {
      step_number = step_number + 1
      next
    }

    for (parameter_file in step$parameter_files) {
      parameters <- yaml::read_yaml(parameter_file)

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














# CREATE BCO
create_bco <- function(
    execution_domain,
    parametric_domain,
    io_domain) {

  biocompute_data <- list(
    object_id = NULL,
    spec_version = NULL,
    etag = NULL,
    provenance_domain = NULL,
    usability_domain = NULL,
    execution_domain = execution_domain,
    parametric_domain = parametric_domain,
    io_domain = list(
      input_subdomain = io_domain$input_subdomain,
      output_subdomain = io_domain$output_subdomain
    ),
    error_domain = list(
      algorithmic_error = NULL,
      empirical_error = NULL
    )
  )

  biocompute_json <- jsonlite::toJSON(biocompute_data, pretty = TRUE, auto_unbox = TRUE)

  return(biocompute_json)
}

