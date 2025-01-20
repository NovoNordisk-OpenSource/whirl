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

# IO DOMAIN
create_io_domain <- function(session_info_rlist) {
  input_subdomain_list <- lapply(session_info_rlist$log_info.read$file, function(file) {
    list(uri = list(uri = file))
  })

  get_output_subdomain_list <- function(output_file_list) {

    output_subdomain_list <- lapply(output_file_list, function(file) {

      if (grepl("\\.html$", file)) {
        mediatype = "text/html"
      } else if (grepl("\\.zip$", file)) {
        mediatype = "application/zip"
      } else if (grepl("\\.csv$", file)) {
        mediatype = "text/csv"
      } else if (grepl("\\txt$", file)) {
        mediatype = "text/txt"
      } else {
        mediatype = " "
      }

      list(
        mediatype = mediatype,
        uri = list(
          filename = basename(file),
          uri = file
        )
      )
    })

    return(output_subdomain_list)
  }
  output_subdomain_list <- get_output_subdomain_list(session_info_rlist$log_info.write$file)

  return(list(
    input_subdomain = input_subdomain_list,
    output_subdomain = output_subdomain_list
  ))
}

# EXECUTION DOMAIN
# Here we should be more dynamic, this setup is more made to fit Bifrost.
create_execution_domain <- function(script, session_info_rlist) {
  execution_domain <- list(
    script = script,
    script_driver = session_info_rlist$environment_options.platform$version,
    software_prerequisites = list(
      list(
        name = "R",
        version = sub("R version ([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", session_info_rlist$environment_options.platform$version)
      ),
      list(
        name = "quarto",
        version = session_info_rlist$environment_options.platform$quarto
      ),
      list(
        name = "pandoc",
        version = session_info_rlist$environment_options.platform$pandoc
      )
    ),
    external_data_endpoints = list(),
    environment_variables = session_info_rlist$environment_options.environment
  )

  return(execution_domain)
}

# PARAMETRIC DOMAIN
create_parametrics_domain <- function(script) {
  # Initialize an empty list to store the dictionaries

  # filtered_variable_names <- ls()[sapply(ls(), function(x) {
  #   obj <- get(x)
  #   typeof(obj) %in% c("character", "double", "integer")
  # })]
  #
  # parametric_domain_list <- list()
  #
  # # Loop over the filtered_variable_names and create dictionaries
  # for (var_name in filtered_variable_names) {
  #   value <- get(var_name)  # Get the value of the variable
  #   parametric_domain_list <- c(
  #     parametric_domain_list,
  #     list(
  #       param = var_name,
  #       value = as.character(value),
  #       step = "0"
  #     )
  #   )
  # }
  #
  # return(parametric_domain_list)
  return(list())
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



script <- "../../azure-projects/7519-bioinformatics-pipeline/proteomics/process_olink_prod_data.R"
obje <- whirl::run(
  script,
  out_formats = "json",
  track_files = TRUE #,
  # biocompute_object = TRUE, ?? default = FALSE. If true ignore out_formats?
  # descriptive_domains = descriptive_domains.json ?? <- this should be a optional input, but a required input if bco = TRUE
)

session_info_rlist <- obje$result[[1]]$session_info_rlist

io_domain <- create_io_domain(session_info_rlist)
execution_domain <- create_execution_domain(obje$script, session_info_rlist)
parametric_domain <- create_parametrics_domain()


biocompute_json <- create_bco(
  execution_domain,
  parametric_domain,
  io_domain
)

# Write the JSON data to a file
write(biocompute_json, "biocompute/biocompute.json")

