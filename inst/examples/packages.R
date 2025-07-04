
# This script produces no errors or warnings for example purposes

message("this script has no errors or warnings")

#Library calls
library(dplyr)

for (package in c("reticulate", "rlang")) {
  library(package, character.only = TRUE)
}


#Load into namespace
ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) +
  ggplot2::geom_point()

zephyr::msg("Test message with zephyr")



