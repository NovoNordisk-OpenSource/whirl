
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whirl

<!-- badges: start -->

[![R-CMD-check](https://github.com/NN-OpenSource/whirl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NN-OpenSource/whirl/actions/workflows/R-CMD-check.yaml)

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

## Usage

The main function in the whirl package is `run()` which takes an `input`
argument that defines the scripts to be executed.

The simplest way is to provide the path to a single script:

``` r
library(whirl)
script <- system.file("examples/simple/success.R", package = "whirl")
run(script)
#> ✔ success.R: Completed succesfully
```

It is also possible to run several scripts simultaneously:

``` r
scripts <- system.file("examples/simple", c("success.R", "warning.R"), package = "whirl")
result <- run(scripts, n_workers = 2)
#> ✔ success.R: Completed succesfully
#> ⚠ warning.R: Completed with warnings
```

Here we are specifying that `run()` can use up to two simultaneous
workers to execute the scripts, meaning that they will be executed in
parallel.

When using `run()` the following files are created:

1.  Creates a log in the same directory as the script with the names
    `{script_name}_log.html`.
2.  Creates a summary log with the overall status of each script.
    Default path is `summary.html`.

Apart from this the function also returns a `tibble` with the status of
the script execution similar to the content of the summary above:

``` r
print(result)
#> # A tibble: 2 × 5
#>      id tag   script                                         status result      
#>   <dbl> <chr> <chr>                                          <chr>  <list>      
#> 1     1 <NA>  /home/oath/R/x86_64-pc-linux-gnu-library/4.4/… succe… <named list>
#> 2     2 <NA>  /home/oath/R/x86_64-pc-linux-gnu-library/4.4/… warni… <named list>
```

## Config files

`run()` also supports running scripts in several sequential steps. This
setup is very useful when your projects have several steps that depends
on each others output, and thereby need to be executed in a specific
order. The best way to implement this in your project is use a
configuration file for whirl. The configuration file is a `yaml` file
that specifies each steps:

``` r
config <- system.file("examples/simple/_whirl.yaml", package = "whirl") 
cat(readLines(config), sep = "\n")
#> steps:
#>   - name: "First step"
#>     paths:
#>       - "success.R"
#>   - name: "Second step"
#>     paths:
#>       - "warning.R"
#>       - "error.R"
```

Here we are specifying that in the first step we run `succes.R`. And
then when this step has been completed we continue to running the
scripts in the second steps.

``` r
result <- run(config, n_workers = 2)
#> ✔ success.R: Completed succesfully
#> ⚠ warning.R: Completed with warnings
#> ✖ error.R: Completed with errors
```

``` r
print(result)
#> # A tibble: 3 × 5
#>      id tag   script                                         status result      
#>   <dbl> <chr> <chr>                                          <chr>  <list>      
#> 1     1 <NA>  /home/oath/R/x86_64-pc-linux-gnu-library/4.4/… succe… <named list>
#> 2     2 <NA>  /home/oath/R/x86_64-pc-linux-gnu-library/4.4/… warni… <named list>
#> 3     3 <NA>  /home/oath/R/x86_64-pc-linux-gnu-library/4.4/… error  <named list>
```

## Customize run()

For more information about how to customize the the execution and the
logging for your needs see the following:

- `run()`: For further information on how to call it.
- `vignette("whirl")`: For a more in depth explanation, and more
  advanced usage.
- `options()`: On how to change the default behavior of whirl.
