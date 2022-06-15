
test_that("list_decks works", {
  suppressMessages(expect_message(list_decks(), "Available flashcard decks"))
  suppressMessages(expect_message(list_decks(), "Data types"))
  expect_silent(test_decks <- list_decks(quiet = TRUE))
  expect_invisible(list_decks(quiet = TRUE))
  # test_decks <- list_decks(quiet = TRUE)
  expect_equal(test_decks$decklabels[1], "data_types")
  expect_equal(test_decks$decktitles[1], "Data types")
  expect_equal(test_decks$decks[1], "Data types (data_types)")
  suppressMessages(expect_error(list_decks(123), "No decks match the pattern entered"))

})

test_that("choose_decks works", {
  # expect_error(choose_deck(letters))
  f <- file()
  options(mypkg.connection = f)
  choice <- paste(c(1, 0, 999), collapse = "\n")
  write(choice, f)
  suppressMessages(expect_message(choose_deck(), "Creating Data types deck"))
  suppressMessages(expect_message(choose_deck(), "No deck selected"))
  suppressMessages(expect_error(choose_deck(), "That response was not valid"))
  options(mypkg.connection = stdin())
  close(f)
})

test_that("get_title works", {
  expect_equal(get_title("inst/extdata/data_types.csv"), "Data types")
})
