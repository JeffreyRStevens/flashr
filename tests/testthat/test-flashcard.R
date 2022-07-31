test_that("validations pass", {
  testdeck <- read.csv(test_path("testdata", "operators.csv"))
  testdeck2 <- testdeck
  names(testdeck2) <- NULL
  expect_error(validate_deck(letters))
  expect_error(validate_deck("test"))
  expect_error(validate_deck(1:5))
  expect_error(validate_deck(testdeck2))
  testdeck3 <- testdeck[, 1:3]
  write.csv(testdeck3, test_path("testdata", "testdeck3.csv"))
  expect_message(validate_deck(test_path("testdata", "testdeck3.csv"),
    package = FALSE
  ), "so using filename for title")
  suppressMessages(expect_message(validate_deck(
    test_path("testdata", "testdeck3.csv"),
    package = FALSE
  )))
  testdeck4 <- testdeck[, 1:2]
  write.csv(testdeck4, test_path("testdata", "testdeck4.csv"))
  expect_message(validate_deck(test_path("testdata", "testdeck4.csv"),
    package = FALSE
  ), "so using filename for title")
  suppressMessages(expect_message(validate_deck(
    test_path("testdata", "testdeck4.csv"),
    package = TRUE
  ), "This deck does not include a package column"))
  testdeck5 <- testdeck[, 1]
  write.csv(testdeck5, test_path("testdata", "testdeck5.csv"))
  file.remove(test_path("testdata", "testdeck3.csv"))
  file.remove(test_path("testdata", "testdeck4.csv"))
  file.remove(test_path("testdata", "testdeck5.csv"))
  testflash <- validate_deck(test_path("testdata", "operators.csv"),
    package = TRUE
  )
  expect_true("title" %in% names(testflash))
})

test_that("fonts are specified properly", {
  suppressMessages(expect_error(
    flashcard("data_types", fontsize = "large"),
    NA
  ))
  suppressMessages(expect_error(flashcard("data_types", fontsize = "100%"), NA))
  suppressMessages(expect_error(
    flashcard("data_types", fontsize = "100"),
    "The `fontsize` value is invalid"
  ))

  suppressMessages(expect_error(
    flashcard("data_types", fontcolor = "Aqua"),
    NA
  ))
  suppressMessages(expect_error(
    flashcard("data_types", fontcolor = "#000000"),
    NA
  ))
  suppressMessages(expect_error(
    flashcard("data_types", fontcolor = "tann"),
    "The `fontcolor` tann is not a valid color"
  ))
  suppressMessages(expect_error(
    flashcard("data_types", linkcolor = "Aqua"),
    NA
  ))
  suppressMessages(expect_error(
    flashcard("data_types", linkcolor = "#000000"),
    NA
  ))
  suppressMessages(expect_error(
    flashcard("data_types", linkcolor = "tann"),
    "The `linkcolor` tann is not a valid color"
  ))
})

test_that("output files are HTML", {
  suppressMessages(expect_error(
    flashcard("data_types", file = "mytest.HTML"),
    NA
  ))
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTM")))
  file.remove("mytest.HTML")
})

test_that("terms select properly", {
  suppressMessages(expect_error(select_terms(c("c()", "%>%")), NA))
  suppressMessages(expect_error(
    select_terms(c("c", "%>%")),
    "The following entry does not include"
  ))
  suppressMessages(expect_error(
    select_terms(c("c", "attr", "%>%")),
    "The following entries do not include"
  ))
  suppressMessages(expect_warning(
    select_terms(c("asdfglkjdf()")),
    "The following entry was not found in the list of functions"
  ))
  suppressMessages(expect_invisible(select_terms(c("c()", "%>%"))))
})

test_that("decks are created properly", {
  suppressMessages(expect_error(create_deck(c("c()", "%>%")), NA))
})
