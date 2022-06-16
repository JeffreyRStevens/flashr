
test_that("list_decks works", {
  suppressMessages(expect_message(list_decks(), "Available flashcard decks"))
  suppressMessages(expect_message(list_decks(), "Data types"))
  expect_silent(list_decks(quiet = TRUE))
})

test_that("choose_decks works", {
  # expect_error(choose_deck(letters))
  # suppressMessages(expect_message(choose_deck(), "Available flashcard decks"))
})

test_that("get_decks works", {
  test_decks <- list_decks(quiet = TRUE)
  expect_equal(test_decks$decklabels[1], "data_types")
  expect_equal(test_decks$decktitles[1], "Data types")
  expect_equal(test_decks$decks[1], "Data types (data_types)")
  expect_invisible(list_decks(quiet = TRUE))
})

test_that("get_title works", {
  expect_equal(get_title("inst/extdata/data_types.csv"), "Data types")
})
