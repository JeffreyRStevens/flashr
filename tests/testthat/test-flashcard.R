test_that("checks work correctly", {
  skip_on_cran()
  expect_error(
    flashcard("data_types", termsfirst = 1),
    "`termsfirst` must be a logical \\(TRUE or FALSE\\)"
  )
  expect_error(
    flashcard("data_types", package = 1),
    "`package` must be a logical \\(TRUE or FALSE\\)"
  )
  expect_error(
    flashcard("data_types", theme = 1),
    "`theme` must be a character"
  )
  expect_error(
    flashcard("data_types", theme = "t"),
    "`theme` must be one of the allowed values: default, black, white, league"
  )
  expect_error(
    flashcard("data_types", file = 1),
    "`file` must be a character"
  )
  expect_error(
    flashcard("data_types", random = 1),
    "`random` must be a logical \\(TRUE or FALSE\\)"
  )
  expect_error(
    flashcard("data_types", fontsize = 1),
    "`fontsize` must be a character"
  )
  expect_error(
    flashcard("data_types", fontcolor = 1),
    "`fontcolor` must be a character"
  )
  expect_error(
    flashcard("data_types", linkcolor = 1),
    "`linkcolor` must be a character"
  )
  expect_error(
    flashcard("data_types", use_browser = 1),
    "`use_browser` must be a logical \\(TRUE or FALSE\\)"
  )
  expect_error(
    flashcard("data_types", omit_na = 1),
    "`omit_na` must be a logical \\(TRUE or FALSE\\)"
  )
  expect_error(
    create_deck(x = c("as_tibble()", "bind_rows()", "c()"), title = 1),
    "`title` must be a character"
  )
})

test_that("validations pass", {
  skip_on_cran()
  testdeck <- read.csv(test_path("testdata", "operators.csv"))
  testdeck2 <- testdeck
  names(testdeck2) <- NULL
  expect_error(
    validate_deck(letters, pkg = TRUE),
    "This deck is not recognized as a available deck or a valid data frame or CSV file."
  )
  expect_error(
    validate_deck("test", pkg = TRUE),
    "This deck is not recognized as a available deck or a valid data frame or CSV file."
  )
  expect_error(
    validate_deck(1:5, pkg = TRUE),
    "This deck is not recognized as a available deck or a valid data frame or CSV file."
  )
  expect_error(
    validate_deck(testdeck2, pkg = TRUE),
    "This data frame does not have term column."
  )
  names(testdeck2)[1] <- "term"
  expect_error(
    validate_deck(testdeck2, pkg = TRUE),
    "This data frame does not have description column."
  )
  names(testdeck2) <- c("term", "description", "package", "b")
  expect_message(validate_deck(testdeck2, pkg = TRUE, omit = TRUE), "No title column, so using testdeck2 for title.")
  names(testdeck2) <- c("term", "description", "package", "title")
  expect_no_message(validate_deck(testdeck2, pkg = TRUE, omit = TRUE))
  testdeck3 <- testdeck[, 1:3]
  write.csv(testdeck3, test_path("testdata", "testdeck3.csv"))
  expect_message(validate_deck(test_path("testdata", "testdeck3.csv"),
    pkg = FALSE, omit = TRUE
  ), "so using filename for title")
  suppressMessages(expect_message(validate_deck(
    test_path("testdata", "testdeck3.csv"),
    pkg = FALSE, omit = TRUE
  )))
  testdeck4 <- testdeck[, -3]
  write.csv(testdeck4, test_path("testdata", "testdeck4.csv"))
  expect_no_message(validate_deck(test_path("testdata", "testdeck4.csv"),
    pkg = FALSE, omit = TRUE
  ))
  suppressMessages(expect_message(validate_deck(
    test_path("testdata", "testdeck4.csv"),
    pkg = TRUE, omit = TRUE
  ), "This deck does not include a"))
  testdeck5 <- testdeck[, -4]
  write.csv(testdeck5, test_path("testdata", "testdeck5.csv"))
  expect_message(validate_deck(test_path("testdata", "testdeck5.csv"),
    pkg = TRUE, omit = TRUE
  ), "so using filename for title")
  # testdeck6 <- testdeck[, 1]
  # write.csv(testdeck6, test_path("testdata", "testdeck6.csv"))
  file.remove(test_path("testdata", "testdeck3.csv"))
  file.remove(test_path("testdata", "testdeck4.csv"))
  file.remove(test_path("testdata", "testdeck5.csv"))
  testflash <- validate_deck(test_path("testdata", "operators.csv"),
    pkg = TRUE, omit = TRUE
  )
  expect_true("title" %in% names(testflash))
  expect_equal(nrow(validate_deck(ex_function_df1, pkg = TRUE, omit = TRUE)), 5)
  expect_equal(nrow(validate_deck(ex_function_df1, pkg = TRUE, omit = FALSE)), 9)
})

test_that("decks are created correctly", {
  expect_no_error(create_deck(x = c("as_tibble()", "bind_rows()", "c()")))
})

test_that("fonts are specified properly", {
  skip_on_cran()
  suppressMessages(expect_no_error(flashcard("data_types", fontsize = "small")))
  suppressMessages(expect_no_error(flashcard("data_types", fontsize = "large")))
  suppressMessages(expect_no_error(flashcard("data_types", fontsize = "100%")))
  suppressMessages(expect_error(flashcard("data_types", fontsize = "100"), "The `fontsize` value is invalid"))

  suppressMessages(expect_no_error(flashcard("data_types", fontcolor = "Aqua")))
  suppressMessages(expect_no_error(flashcard("data_types", fontcolor = "#000000")))
  suppressMessages(expect_error(flashcard("data_types", fontcolor = "tann"), "The `fontcolor` tann is not a valid color"))
  suppressMessages(expect_no_error(flashcard("data_types", linkcolor = "Aqua")))
  suppressMessages(expect_no_error(flashcard("data_types", linkcolor = "#000000")))
  suppressMessages(expect_error(flashcard("data_types", linkcolor = "tann"), "The `linkcolor` tann is not a valid color"))
})

test_that("output files are HTML", {
  skip_on_cran()
  suppressMessages(expect_no_error(flashcard("data_types", file = "mytest.Rmd")))
  suppressMessages(expect_no_error(flashcard("data_types", file = "mytest.html")))
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTM")))
  file.remove("mytest.Rmd")
  file.remove("mytest.html")
})
