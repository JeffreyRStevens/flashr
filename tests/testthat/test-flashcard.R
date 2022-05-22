test_that("validations pass", {
  testdeck <- read.csv("operators.csv")
  testdeck2 <- testdeck
  names(testdeck2) <- NULL
  expect_error(validate_deck(letters))
  expect_error(validate_deck("test"))
  expect_error(validate_deck(1:5))
  expect_error(validate_deck(testdeck2))
  testdeck3 <- testdeck[, 1:2]
  write.csv(testdeck3, "testdeck3.csv")
  suppressMessages(expect_message(validate_deck("testdeck3.csv", package = FALSE)))
  testdeck4 <- testdeck[, 1]
  write.csv(testdeck4, "testdeck4.csv")
  suppressMessages(expect_error(validate_deck("testdeck4.csv", package = FALSE)))
  file.remove(test_path("testdeck3.csv"))
  file.remove(test_path("testdeck4.csv"))
})
