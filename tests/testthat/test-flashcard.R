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
  suppressMessages(expect_error(validate_deck(
    test_path("testdata", "testdeck4.csv"),
    package = TRUE
  )))
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

test_that("output files are HTML", {
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTML"), NA))
  suppressMessages(expect_error(flashcard("data_types", file = "mytest.HTM")))
  file.remove("mytest.HTML")
})
