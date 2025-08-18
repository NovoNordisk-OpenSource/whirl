
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whirl

<!-- badges: start -->

[![Checks](https://github.com/NovoNordisk-OpenSource/whirl/actions/workflows/check_and_co.yaml/badge.svg)](https://github.com/NovoNordisk-OpenSource/whirl/actions/workflows/check_and_co.yaml)

[![Codecov test
coverage](https://codecov.io/gh/NovoNordisk-OpenSource/whirl/graph/badge.svg)](https://app.codecov.io/gh/NovoNordisk-OpenSource/whirl)

[![CRAN
status](https://www.r-pkg.org/badges/version/whirl)](https://CRAN.R-project.org/package=whirl)

[<img src="http://pharmaverse.org/shields/whirl.svg">](https://pharmaverse.org)

<!-- badges: end -->

## Overview

The whirl package provide functionalities for executing scripts in batch
and simultaneously getting a log from the individual executions. A log
from script execution is in many pharmaceutical companies a GxP
requirement, and the whirl package honors this requirement by generating
a log that, among other things, contains information about:

- Status (did the script run with any error or warnings)
- The actual code itself
- Date and time of execution
- The environment the script was executed under (session info)
- Information about packages versions that was utilized
- Environmental variables

And all this is wrapped into a nicely formatted html document that is
easy to navigate.

## Installation

``` r
# Install the released version from CRAN:
install.packages("whirl")
# Install the development version from GitHub:
pak::pak("NovoNordisk-OpenSource/whirl")
```

## Usage

The main function in the whirl package is `run()` which takes an `input`
argument that defines the scripts to be executed.

The simplest way is to provide the path to a single script:

``` r
library(whirl)

run("success.R")
#> ✔ success.R: Completed succesfully
```

It is also possible to run several scripts simultaneously:

``` r
result <- run(c("success.R", "warning.R"), n_workers = 2)
#> ✔ success.R: Completed succesfully
#> ⚠ warning.R: Completed with warnings
```

Here we are specifying that `run()` can use up to two simultaneous
workers to execute the scripts, meaning that they will be executed in
parallel.

When using `run()` the following files are created:

1.  Creates a log in the same directory as the script with the names
    `{script_name}_log.html`. See
    [example_log.html](https://novonordisk-opensource.github.io/whirl/articles/example_log.html)
    for an example of a simple log.
2.  Creates a summary log with the overall status of each script.
    Default path is `summary.html`. See
    [summary.html](https://novonordisk-opensource.github.io/whirl/articles/summary.html)
    for an example of a summary of the same log as above.

Apart from this the function also returns a `tibble` with the status of
the script execution similar to the content of the summary above:

``` r
print(result)
#> # A tibble: 2 × 6
#>      id tag    script                                status result       log_dir
#>   <dbl> <chr>  <chr>                                 <chr>  <list>       <chr>  
#> 1     1 Step 1 /private/var/folders/fx/71by3f551qzb… succe… <named list> /priva…
#> 2     2 Step 1 /private/var/folders/fx/71by3f551qzb… warni… <named list> /priva…
```

## Config files

`run()` also supports running scripts in several sequential steps. This
setup is very useful when your projects have several steps that depends
on each others output, and thereby need to be executed in a specific
order. The best way to implement this in your project is use a
configuration file for whirl. The configuration file is a `yaml` file
that specifies each steps:

`_whirl.yaml:`

``` yaml
steps:
  - name: "First step"
    paths:
      - "success.R"
  - name: "Second step"
    paths:
      - "warning.R"
      - "error.R"``
```

Here we are specifying that in the first step we run `succes.R`. And
then when this step has been completed we continue to running the
scripts in the second steps.

``` r
result <- run("_whirl.yaml", n_workers = 2)
#> ✔ success.R: Completed succesfully
#> ✖ error.R: Completed with errors
#> ⚠ warning.R: Completed with warnings
```

``` r
print(result)
#> # A tibble: 3 × 6
#>      id tag         script                           status result       log_dir
#>   <dbl> <chr>       <chr>                            <chr>  <list>       <chr>  
#> 1     1 First step  /private/var/folders/fx/71by3f5… succe… <named list> /priva…
#> 2     2 Second step /private/var/folders/fx/71by3f5… warni… <named list> /priva…
#> 3     3 Second step /private/var/folders/fx/71by3f5… error  <named list> /priva…
```

## Useful links

For more information about how to customize the the execution and the
logging for your needs see the following:

- `run()`: For further information on how to call it.
- `vignette("whirl")`: For a more in depth explanation, and more
  advanced usage.
- `vignette("articles/example")`: With a simple example, including the
  created log.
- `help("whirl-options")`: On how to change the default behavior of
  whirl.
- [NovoNordisk-OpenSource/R-packages](https://novonordisk-opensource.github.io/R-packages/)
  for an overview of connector and other R packages published by Novo
  Nordisk.
