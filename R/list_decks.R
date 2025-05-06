#' List available available flashcard decks
#'
#' @description This function searches for flashcard decks stored in GitHub
#' repositories. By default, the function searches the
#' [flashr_decks repo](https://github.com/JeffreyRStevens/flashr_decks/). But
#' other GitHub repos can be used.
#'
#' To narrow the results, include text in the `pattern` argument (for example,
#' `list_decks(pattern = "r4ds")`).
#'
#' @details You are welcome to fork the
#' [flashr_decks repo](https://github.com/JeffreyRStevens/flashr_decks/) and
#' modify or add your own decks. Or you can create your own repo from scratch.
#' Just make sure to place your decks in a directory called `decks/` in your
#' root directory. Then set the `repo` argument to your username and repo (see
#' Examples).
#'
#' @param pattern String pattern to search in list of decks.
#' @param repo GitHub username and repo for deck repository in the format
#' of "username/repository". Default value is "JeffreyRStevens/flashr_decks".
#' @param quiet Logical to prevent list information from printing to console.
#'
#' @return
#' Outputs a list of available built-in flashcard decks to the console.
#' @export
#'
#' @importFrom memoise memoise
#'
#' @family functions for finding decks
#'
#' @examplesIf interactive()
#' # View all available decks
#' list_decks()
#'
#' # View decks with text matching pattern
#' list_decks(pattern = "r4ds")
#'
#' # View decks from specific repository
#' list_decks(repo = "JeffreyRStevens/flashr_decks")
list_decks <- function(pattern = NULL,
                       repo = "JeffreyRStevens/flashr_decks",
                       quiet = FALSE) {
  # Check arguments
  check_character("pattern", pattern, nullok = TRUE)
  check_character("repo", repo)
  check_logical("quiet", quiet)

  # Get decks
  if (repo == "JeffreyRStevens/flashr_decks") {
    decks_repo <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr_decks/main/decks/00_all_decks.csv"
    fail_gracefully(decks_repo)
    all_decks <- utils::read.csv(decks_repo)
    decks <- all_decks$deck
    titles <- all_decks$title
    decklabels <- all_decks$decklabel
  } else {
    # Get contents of decks/ directory
    repo_text <- paste0("GET /repos/", repo, "/contents/decks")
    deckfiles <- get_repo_mem(repo_text)

    # Create labels, paths, and titles for decks
    deckpaths <- paste0(
      "https://raw.githubusercontent.com/", repo,
      "/main/decks/", deckfiles
    )
    decklabels <- gsub(".csv", "", deckfiles)
    titles <- vapply(deckpaths, get_title_mem, character(1))
    decks <- paste0(titles, " (", decklabels, ")")
  }

  # Search text of decks for patterns
  if (!is.null(pattern)) {
    if (!is.character(pattern)) {
      pattern <- deparse(substitute(pattern))
    }
    deck_nums <- grep(pattern, decks, ignore.case = TRUE)
    decks <- decks[deck_nums]
    decklabels <- decklabels[deck_nums]
    titles <- titles[deck_nums]
    if (length(decks) == 0) {
      cli::cli_abort(
        "No decks match the pattern entered. Try another pattern string."
      )
    }
  }

  # Return list of decks to console
  if (!quiet) {
    cli::cli_text("Available flashcard decks")
    cli::cli_ol(decks)
  }

  # Invisibly return decks, labels, and titles
  invisible(list(
    decklabels = decklabels,
    decktitles = unname(titles),
    decks = decks
  ))
}

#' Choose from available flashcard decks
#'
#' @description This function prints a list of flashcard decks to the console
#' and let's the user choose one of the decks. By default, the function searches
#' the [flashr_decks repo](https://github.com/JeffreyRStevens/flashr_decks/).
#' But other GitHub repos can be used.
#'
#' To narrow the results, include text in the `pattern` argument (for example,
#' `choose_deck(pattern = "r4ds")`).
#'
#' @param pattern String pattern to search in list of decks.
#' @param choice Integer value of choice from list of decks if you already
#' know which deck you would like to use without listing again.
#' @param repo GitHub username and repo for deck repository in the format
#' of "username/repository". Default value is "JeffreyRStevens/flashr_decks".
#'
#' @return
#' Outputs a list of available built-in flashcard decks to the console, where
#' the user can choose one of the decks to generate flashcards.
#' @export
#'
#' @note
#' This function **requires internet connectivity** as it checks GitHub repos
#' for decks.
#'
#' @family functions for finding decks
#'
#' @examplesIf interactive()
#' \dontrun{
#' # Choose from all available decks in default repository
#' choose_deck()
#'
#' # Choose from decks including text matching pattern
#' choose_deck(pattern = "r4ds")
#'
#' # Choose from decks from specific repository
#' choose_deck(repo = "JeffreyRStevens/flashr_decks")
#' }
choose_deck <- function(pattern = NULL,
                        choice = NULL,
                        repo = "JeffreyRStevens/flashr_decks") {
  # Check arguments
  check_character("pattern", pattern, nullok = TRUE)
  check_character("repo", repo)

  # If no choice is passed to function
  if (is.null(choice)) {
    # List decks
    deck_list <- list_decks(pattern = pattern, repo = repo)

    # Record choice
    cat("Please enter the number for a deck or 0 to exit: ")
    choice <- as.numeric(readLines(con = getOption("mypkg.connection"), n = 1))
  }

  # Check if integer entered for choice
  if (!is.numeric(choice)) {
    cli::cli_abort("Please enter an integer for the `choice` argument.")
  } else if (choice %% 1 != 0) {
    cli::cli_abort("Please enter an integer for the `choice` argument.")
  }
  deck_list <- list_decks(pattern = pattern, repo = repo, quiet = TRUE)

  # Extract decks, labels, and title
  decks <- deck_list$decks
  decklabels <- deck_list$decklabels
  titles <- deck_list$decktitles

  # Print deck name and create flashcard deck, exit, or abort for invalid decks
  if (choice %in% seq_len(length(decks))) {
    cli::cli_text(
      "Creating {.field ",
      {
        unname(titles[choice])
      },
      "} deck."
    )
    flashcard(decklabels[choice])
  } else if (identical(choice, 0)) {
    cli::cli_text("No deck selected.")
  } else {
    cli::cli_abort("That response was not valid. Please rerun `choose_deck()` and enter a valid number for an available deck.")
  }
}

# Extract title from CSV
get_title <- function(x) {
  data <- utils::read.csv(x)
  data$title[1]
}
get_title_mem <- memoise::memoise(get_title)

# Get file names in repository
get_repo <- function(repo_text) {
  x <- gh::gh(repo_text)
  vapply(x, "[[", "", "name")
}
get_repo_mem <- memoise::memoise(get_repo)
