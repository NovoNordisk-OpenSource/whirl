
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtemplate

<!-- badges: start -->

[![R-CMD-check](https://github.com/NN-OpenSource/Rtemplate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NN-OpenSource/Rtemplate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package is a template for future R packages developed by the ATMOS
team.

To create a new package press “Use this template” and create a new
repository based on this template.

The template sets branch policies, package development standards, and
GitHub actions. For details see below:

## Branch policies

The following applies to the `main` branch:

- Requires a pull request before merging
- Pull requests has to be approved by 2 reviewers
- Pull requests has to be reviewed by at least one code owner
- All conversations on code must be resolved before a pull request can
  be merged
- When merging pull requests only merge and squash commits are enabled
- TODO: Requirement of linked issues

## Standards

- Set licence to Apache licence version 2.0 with Novo Nordisk A/S as the
  copyright holder. Check that year is correct!
- Use a README.Rmd
- Use `testthat` for testing
- Use `pkgdown` to create a documentation webpage
- Setup `lintr` with default style

## GitHub actions (todo)

- Checks: (or should we enforce on main branch and check on others/PRs?)
  - styling: `lintr::lint_package()`
  - updated documentation
  - updated README
- R CMD Check
- Test coverage
- Pkgdown webpage hosted on GitHub pages
- First time update to new package name

## Design (todo)

- Some great design guide to be followed and include some awesome CSS
