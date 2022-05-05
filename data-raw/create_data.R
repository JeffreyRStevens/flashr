library(here)
data_types <- read.csv(here("inst/extdata/data_types.csv"))
usethis::use_data_raw(name = "data_types")
vectors <- read.csv(here("inst/extdata/vectors.csv"))
usethis::use_data_raw(name = "vectors")

usethis::use_data(data_types, vectors, overwrite = TRUE)
