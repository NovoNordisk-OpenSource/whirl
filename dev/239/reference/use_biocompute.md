# Use whirl to create biocompute logs

Utility function to setup execution with whirl in your project suitable
for creating biocompute logs with
[`write_biocompute()`](https://novonordisk-opensource.github.io/whirl/reference/write_biocompute.md):

1.  Creates configuration file (default `_whirl.yml`) with default
    values for the `biocompute` metadata.

2.  Updates `.gitignore` to not include log files

See
[`vignette("whirl")`](https://novonordisk-opensource.github.io/whirl/articles/whirl.md)
for how to specify paths inside the configuration file.

## Usage

``` r
use_biocompute(
  config_file = "_whirl.yml",
  parametrics_file = "_parametrics.yml"
)
```

## Arguments

- config_file:

  Path to the whirl config file, relative to the project

- parametrics_file:

  Path to the biocompute parametrics file, relative to the project
