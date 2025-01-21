
ymlfile <- "~/github-projects/whirl/tests/testthat/scripts/_biocompute_parametrics.yml"
paths = yaml::read_yaml(ymlfile) # read config file

writeLines("Hello World!", paths$outputFilePath)

