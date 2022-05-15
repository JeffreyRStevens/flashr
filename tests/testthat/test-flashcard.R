test_that("checks pass", {
  testdeck <- read.csv("../../data/operators.csv")
  testdeck2 <- testdeck
  names(testdeck2) <- NULL
  expect_error(flashcard("../../data/operators.csv"))
  expect_error(flashcard(testdeck2))
  suppressMessages(expect_message(flashcard(file = "../../data/operators2.csv", package = FALSE)))
  suppressMessages(expect_error(flashcard(file = "../../data/operators2.csv", package = TRUE)))
})
