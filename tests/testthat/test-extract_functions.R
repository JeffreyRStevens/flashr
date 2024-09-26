rmd <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"
ex_function_df1 <- data.frame(term = c("set()", "install.packages()",
                                       "install_github()", "library()", "flashcard()",
                                       "flashcard()", "read.csv()"),
                              description = c(NA_character_, "install R packages",
                                              NA_character_, "load R packages",
                                              NA_character_, NA_character_,
                                              "import CSV file (base R)"),
                              package = c(NA_character_, "utils", NA_character_,
                                          "base", NA_character_, NA_character_,
                                          "utils"),
                              title = rep("Test", 7))
ex_function_df2 <- data.frame(term = c("apple()", "apply()", "+"),
                             description = c(NA_character_, "apply a function to multiple elements of an object in base R", "addition"),
                             package = c(NA_character_, "base", "base"),
                             title = rep("Test", 3))

test_that("code is extracted", {
  expect_equal(extract_code(file = rmd)[1], "knitr::opts_chunk$set(")
  expect_equal(extract_code(file = rmd)[15], "flashcard(\"inst/extdata/operators.csv\")" )
  expect_error(extract_code(file = 1),
               "'file' should be a character string with one element")
})

test_that("functions are extracted", {
  expect_equal(extract_functions(extract_code(rmd)),
               c("set", "install.packages", "install_github", "library",
                 "flashcard", "flashcard", "read.csv", "head", "flashcard"))
  expect_error(extract_functions(code = 1),
               "'code' should be a character vector")
})

test_that("function descriptions and packages are retrieved", {
  expect_identical(build_functions_df(file = rmd, title = "Test"),
                   ex_function_df1)
  expect_identical(build_functions_df(fs = c("apple", "apply", "+"), title = "Test"),
                   ex_function_df2)
  expect_error(build_functions_df(title = "Test"),
               "Needs argument for either file or fs but not both")
  expect_error(build_functions_df(file = "x", fs = "y", title = "Test"),
               "Needs argument for either file or fs but not both")
  expect_error(build_functions_df(file = 1, title = "Test"),
               "'file' should be a character string with one element")
  expect_error(build_functions_df(file = c("x", "y"), title = "Test"),
               "'file' should be a character string with one element")
  expect_error(build_functions_df(fs = 1, title = "Test"),
               "'fs' should be a character vector")
  expect_error(build_functions_df(fs = "x", title = 1),
               "'title' should be a character vector")
  expect_error(build_functions_df(fs = "x", title = "Test", desc = 1),
               "'desc' should be a logical")
})
