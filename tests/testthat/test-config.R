withr::with_tempdir(code = {
test_that("execute_with_yaml function test", {
    # Define the script list and copy example scripts to a temporary directory
    scripts_list <- c("prg1.R", "prgRmd.Rmd")
    file.copy(from = system.file("examples", scripts_list, package = "whirl"),
              to = getwd(), overwrite = TRUE)

    # Set up a mock YAML file with parameters for testing
    mock_yaml_file <- file.path(getwd(), "mock_params.yaml")

    cat(
      "params:\n",
      "  paths: [", paste0("'", file.path(getwd(), scripts_list), "'", collapse = ", "), "]\n",
      "  parallel: TRUE\n",
      "  num_cores: !expr future::availableCores()\n",
      "  summary_dir: '", trimws(file.path(getwd(), "summary")), "'\n",
      "  track_files: FALSE\n",
      "  check_renv: FALSE\n",
      "  out_formats: 'html'\n",
      "  approved_pkgs_folder: NULL\n",
      "  approved_pkgs_url: NULL\n",
      "  out_dir: '", trimws(file.path(getwd(), "log")), "'\n",
      file = mock_yaml_file
    )

    # Ensure that the summary directory exists
    dir.create(file.path(getwd(), "log"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "summary"), showWarnings = FALSE)

    # Execute the function with the mock YAML and store the result
    result <- execute_with_yaml(mock_yaml_file, eval.expr = TRUE)

    # Assert that the result is a data frame and not empty
    expect_true(is.data.frame(result), "Result should be a data frame")

    expect_equal(nrow(result), 2)

})

# Empty Paths Test
test_that("execute_with_yaml handles empty paths", {

    mock_yaml_file <- file.path(getwd(), "mock_params_empty.yaml")

    # Create a mock YAML file with empty paths
    cat(
      "params:\n",
      "  paths: []\n",
      "  parallel: TRUE\n",
      "  num_cores: !expr future::availableCores()\n",
      "  summary_dir: '", getwd(), "/summary'\n",
      "  track_files: FALSE\n",
      "  check_renv: FALSE\n",
      "  out_formats: 'html'\n",
      "  approved_pkgs_folder: NULL\n",
      "  approved_pkgs_url: NULL\n",
      "  out_dir: '", getwd(), "/output'\n",
      file = mock_yaml_file
    )

    # Execute the function with the mock YAML and store the result
    expect_error(execute_with_yaml(mock_yaml_file, eval.expr = TRUE))

})

# Non-Existent Paths Test
test_that("execute_with_yaml handles non-existent paths", {
    mock_yaml_file <- file.path(getwd(), "mock_params_nonexistent.yaml")

    # Create a mock YAML file with non-existent paths
    cat(
      "params:\n",
      "  paths: ['/path/does/not/exist/script.R', '/another/nonexistent/script.R']\n",
      "  parallel: TRUE\n",
      "  num_cores: !expr future::availableCores()\n",
      "  summary_dir: '", getwd(), "/summary'\n",
      "  track_files: FALSE\n",
      "  check_renv: FALSE\n",
      "  out_formats: 'html'\n",
      "  approved_pkgs_folder: NULL\n",
      "  approved_pkgs_url: NULL\n",
      "  out_dir: '", getwd(), "/output'\n",
      file = mock_yaml_file
    )

    # Assert that the result is as expected
    expect_error(execute_with_yaml(mock_yaml_file, eval.expr = TRUE))
})

# Invalid YAML Test
test_that("execute_with_yaml handles invalid YAML", {
    mock_yaml_file <- file.path(getwd(), "mock_params_invalid.yaml")

    # Create a mock YAML file with invalid format
    cat(
      "params: [\n",
      "  paths: '/path/to/script.R'\n",
      "  parallel: TRUE\n",
      "  num_cores: !expr future::availableCores()\n",
      "  summary_dir: '", getwd(), "/summary'\n",
      "  track_files: FALSE\n",
      "  check_renv: FALSE\n",
      "  out_formats: 'html'\n",
      "  approved_pkgs_folder: NULL\n",
      "  approved_pkgs_url: NULL\n",
      "  out_dir: '", getwd(), "/output'\n",
      file = mock_yaml_file
    )

    # Assert that the result is as expected
    expect_error(execute_with_yaml(mock_yaml_file, eval.expr = TRUE))
})
})
