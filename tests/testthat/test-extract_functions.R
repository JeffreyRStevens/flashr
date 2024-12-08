rmd <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"
ex_function_df1 <- data.frame(
  term = c(
    "flashcard()", "head()",
    "install.packages()", "install_github()", "library()",
    "read.csv()", "set()"
  ),
  description = c(
    NA_character_,
    "return first rows of matrix, data frame, etc.",
    "install R packages", NA_character_,
    "load R packages",
    "import CSV file (base R)",
    NA_character_
  ),
  package = c(
    NA_character_, "utils",
    "utils", NA_character_, "base",
    "utils", NA_character_
  ),
  title = rep("Test", 7)
)
ex_function_df1b <- ex_function_df1[!is.na(ex_function_df1$description), ]
ex_function_df2 <- data.frame(
  term = c("+", "apple()", "apply()"),
  description = c(
    "addition", NA_character_,
    "apply a function to multiple elements of an object in base R"
  ),
  package = c("base", NA_character_, "base"),
  title = rep("Test", 3)
)

test_that("code is extracted", {
  expect_equal(extract_code(file = rmd)[1], "knitr::opts_chunk$set(")
  expect_equal(extract_code(file = rmd)[16], "flashcard(\"inst/extdata/operators.csv\")")
  expect_error(
    extract_code(file = 1),
    "'file' should be a character string with one element"
  )
  expect_equal(extract_code(file = rmd)[8], "# install.packages(\"remotes\")")
  expect_equal(extract_code(file = rmd, comments = FALSE)[8], "remotes::install_github(\"JeffreyRStevens/flashr\")")
  expect_equal(length(extract_code(file = rmd)), 16)
  expect_equal(length(extract_code(file = rmd, empty = FALSE)), 15)
})

test_that("functions are extracted", {
  expect_equal(
    extract_functions(extract_code(rmd)),
    c(
      "set", "install.packages", "install_github", "library",
      "flashcard", "flashcard", "read.csv", "head", "flashcard"
    )
  )
  expect_equal(
    extract_functions(extract_code(rmd), duplicates = FALSE),
    c(
      "set", "install.packages", "install_github", "library",
      "flashcard", "read.csv", "head"
    )
  )
  expect_error(
    extract_functions(code = 1),
    "'code' should be a character vector"
  )
})

test_that("function descriptions and packages are retrieved", {
  expect_identical(
    build_functions_df(file = rmd, title = "Test"),
    ex_function_df1b
  )
  expect_identical(
    build_functions_df(file = rmd, title = "Test", omit = FALSE),
    ex_function_df1
  )
  expect_identical(
    build_functions_df(
      fs = c("apple", "apply", "+"),
      title = "Test", omit = FALSE
    ),
    ex_function_df2
  )
  expect_error(
    build_functions_df(title = "Test"),
    "Needs argument for either file or fs but not both"
  )
  expect_error(
    build_functions_df(file = "x", fs = "y", title = "Test"),
    "Needs argument for either file or fs but not both"
  )
  expect_error(
    build_functions_df(file = 1, title = "Test"),
    "'file' should be a character string with one element"
  )
  expect_error(
    build_functions_df(file = c("x", "y"), title = "Test"),
    "'file' should be a character string with one element"
  )
  expect_error(
    build_functions_df(fs = 1, title = "Test"),
    "'fs' should be a character vector"
  )
  expect_error(
    build_functions_df(fs = "x", title = 1),
    "'title' should be a character vector"
  )
  expect_error(
    build_functions_df(fs = "x", title = "Test", desc = 1),
    "'desc' should be a logical"
  )
})
