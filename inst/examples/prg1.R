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
