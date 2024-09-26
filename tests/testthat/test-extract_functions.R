rmd <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"
ex_function_df <- data.frame(term = c("apple()", "apply()", "+"),
                             description = c(NA_character_, "apply a function to multiple elements of an object in base R", "addition"),
                             package = c(NA_character_, "base", "base"),
                             title = rep("Test", 3))

test_that("code is extracted", {
  expect_equal(extract_code(rmd)[1], "knitr::opts_chunk$set(")
  expect_equal(extract_code(rmd)[15], "flashcard(\"inst/extdata/operators.csv\")" )
})

test_that("functions are extracted", {
  expect_equal(extract_functions(extract_code(rmd)),
               c("set", "install.packages", "install_github", "library",
                 "flashcard", "flashcard", "read.csv", "head", "flashcard"))
})

test_that("function descriptions and packages are retrieved", {
  expect_identical(build_functions_file(fs = c("apple", "apply", "+"), title = "Test"),
                   ex_function_df)
  expect_error(build_functions_file(fs = 1, title = "Test"),
               "fs should be a character vector")
  expect_error(build_functions_file(fs = "x", title = 1),
               "title should be a character vector")
  expect_error(build_functions_file(fs = "x", title = "Test", desc = 1),
               "desc should be a logical")
})
