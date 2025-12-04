# Log example

In this example we are going to execute the following script and create
a log of it’s execution:

`example.R:`

``` r
#' Setup

library(dplyr)
library(ggplot2)

#' Prepare data

x <- mtcars |>
  as_tibble(rownames = "car")

print(x)

#' Create and save plot

ggplot(data = x) +
  geom_point(mapping = aes(x = mpg, y = hp, size = wt, colour = as.factor(am)))

ggsave("plot1.png")
```

We are going to use the
[`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md)
function to execute the script, and since this vignette is created on
Linux we can use the `whirl.track_files` option to automatically track
the used files:

``` r
library(whirl)

options(whirl.track_files = TRUE)
options(whirl.verbosity_level = "minimal")
```

The `verbosity_level` is set to `minimal` for nicer printing in this
vignette. Now we are ready to execute the script:

``` r
result <- run("example.R")
#> ✔ example.R: Completed succesfully. See log html.

print(result)
#> # A tibble: 1 × 6
#>      id tag    script                                status result       log_dir
#>   <dbl> <chr>  <chr>                                 <chr>  <list>       <chr>  
#> 1     1 Step 1 /tmp/RtmplsShmr/file1af05f4a0fd5/exa… succe… <named list> /tmp/R…
```

The script is now executed and you can access the logs below:

- [View summary
  log](https://novonordisk-opensource.github.io/whirl/articles/summary.md)
- [View log for
  example.R](https://novonordisk-opensource.github.io/whirl/articles/example_log.md)
- [The saved
  plot1.png](https://novonordisk-opensource.github.io/whirl/articles/plot1.png)
