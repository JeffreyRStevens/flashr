rmd <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"
ex_function_df1 <- data.frame(
  term = c(
    "flashcard()", "head()",
    "install.packages()", "install_github()", "library()", "print()",
    "read.csv()", "readCitationFile()", "set()"
  ),
  description = c(
    NA_character_,
    "return first rows of matrix, data frame, etc.",
    "install R packages", NA_character_,
    "load R packages", "prints argument",
    "import CSV file (base R)", NA_character_,
    NA_character_
  ),
  package = c(
    NA_character_, "utils",
    "utils", NA_character_, "base", "base",
    "utils", NA_character_, NA_character_
  ),
  title = rep("Test", 9)
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
