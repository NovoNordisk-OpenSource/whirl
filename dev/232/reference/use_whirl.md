# Use whirl

Utility function to setup execution with whirl in your project:

1.  Creates configuration file (default `_whirl.yml`)

2.  Updates `.gitignore` to not include log files

See
[`vignette("whirl")`](https://novonordisk-opensource.github.io/whirl/articles/whirl.md)
for how to specify paths inside the configuration file.

## Usage

``` r
use_whirl(config_file = "_whirl.yml")
```

## Arguments

- config_file:

  Path to the whirl config file, relative to the project
