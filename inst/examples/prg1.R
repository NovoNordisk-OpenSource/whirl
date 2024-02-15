
#' Setup

library(tidyverse)

#' Data data

x <- mtcars |>
  as_tibble(rownames = "car")

x |>
  print(n = 100)

#' Plot

ggplot(data = x) +
  geom_point(mapping = aes(x = mpg, y = hp, size = wt, colour = as.factor(am)))

ggsave("plot1.png")
