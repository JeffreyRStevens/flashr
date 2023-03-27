test_that("list_decks works", {
  skip_on_cran()
  suppressMessages(expect_message(list_decks(), "Available flashcard decks"))
  suppressMessages(expect_message(list_decks(), "Data types"))
  expect_silent(test_decks <- list_decks(quiet = TRUE))
  expect_invisible(list_decks(quiet = TRUE))
  expect_equal(test_decks$decklabels[1], "data_types")
  expect_equal(test_decks$decktitles[1], "Data types")
  expect_equal(test_decks$decks[1], "Data types (data_types)")
  suppressMessages(expect_error(
    list_decks(123),
    "No decks match the pattern entered"
  ))
})

test_that("choose_decks works", {
  skip_on_cran()
  expect_error(choose_deck(choice = "3"), "Please enter an integer")
  expect_error(choose_deck(choice = 3.3), "Please enter an integer")
  expect_message(choose_deck(choice = 1), "Data types")
  expect_message(choose_deck(choice = 0), "No deck selected")
  expect_error(choose_deck(choice = 999), "That response was not valid")
  f <- file()
  options(mypkg.connection = f)
  choice <- 999
  write(choice, f)
  suppressMessages(expect_error(choose_deck(), "That response was not valid"))
  options(mypkg.connection = stdin())
  close(f)
})

test_that("get_title works", {
  skip_on_cran()
  expect_equal(get_title("inst/extdata/data_types.csv"), "Data types")
})

test_that("get_repo works", {
  skip_on_cran()
  repo_text <- paste0("GET /repos/JeffreyRStevens/flashr_decks/contents/decks")
  repo_files <- get_repo_mem(repo_text)
  expect_equal(repo_files[1], "00_all_decks.csv")
})
