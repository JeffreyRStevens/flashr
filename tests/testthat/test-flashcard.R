test_that("validations pass", {
  skip_on_cran()
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
  skip_on_cran()
  suppressMessages(expect_error(flashcard("data_types", fontsize = "large"), NA))
  suppressMessages(expect_error(flashcard("data_types", fontsize = "100%"), NA))
  suppressMessages(expect_error(flashcard("data_types", fontsize = "100"), "The `fontsize` value is invalid"))

  suppressMessages(expect_error(flashcard("data_types", fontcolor = "Aqua"), NA))
  suppressMessages(expect_error(flashcard("data_types", fontcolor = "#000000"), NA))
  suppressMessages(expect_error(flashcard("data_types", fontcolor = "tann"), "The `fontcolor` tann is not a valid color"))
  suppressMessages(expect_error(flashcard("data_types", linkcolor = "Aqua"), NA))
  suppressMessages(expect_error(flashcard("data_types", linkcolor = "#000000"), NA))
  suppressMessages(expect_error(flashcard("data_types", linkcolor = "tann"), "The `linkcolor` tann is not a valid color"))
})

test_that("output files are HTML", {
  skip_on_cran()
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTML"), NA))
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTM")))
  file.remove("mytest.HTML")
})
