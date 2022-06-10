library(here)
data_types <- read.csv(here::here("inst/extdata/data_types.csv"))
attr(data_types, "title") <- data_types$title[1]
usethis::use_data_raw(name = "data_types")

vectors <- read.csv(here("inst/extdata/vectors.csv"))
attr(vectors, "title") <- vectors$title[1]
usethis::use_data_raw(name = "vectors")

usethis::use_data(data_types, vectors, overwrite = TRUE)

file.copy("data/operators.csv", "tests/testthat/testdata/operators.csv", overwrite = TRUE)
operators <- read.csv(here::here("data/operators.csv"))
file.copy("inst/extdata/data_types.csv", "tests/testthat/inst/extdata/data_types.csv", overwrite = TRUE)

