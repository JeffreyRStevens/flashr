test_that("checks pass", {
  testdeck <- read.csv("operators.csv")
  testdeck2 <- testdeck
  names(testdeck2) <- NULL
  expect_error(flashcard("operators.csv"))
  expect_error(flashcard(testdeck2))
  testdeck3 <- testdeck[, 1:2]
  withr::local_file(write.csv(testdeck3, "testdeck3.csv"))
  suppressMessages(expect_message(flashcard(file = "testdeck3.csv", package = FALSE)))
  suppressMessages(expect_error(flashcard(file = "testdeck3.csv", package = TRUE)))
})
