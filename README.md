
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

## How to

The `path` argument in `run()` currently supports single scripts,
multiple scripts, folders, and/or config files (more about that further
down).

## Config files
