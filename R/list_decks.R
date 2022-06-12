
#' List available available flashcard decks
#'
#' @description This function searches for flashcard decks stored in GitHub
#' repositories. By default, the function searches the
#' [flashr_decks repo]("https://github.com/JeffreyRStevens/flashr_decks/"). But
#' other GitHub repos can be used.
#'
#' To narrow the results, include text in the `pattern` argument (for example,
#' `list_decks(pattern = "r4ds")`).
#'
#' @details You are welcome to fork the
#' [flashr_decks repo]("https://github.com/JeffreyRStevens/flashr_decks/") and
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
#' @family functions for finding decks
#'
#' @examples
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
  repo_text <- paste0("GET /repos/", repo, "/contents/decks")
  deckfiles <- gh::gh(repo_text) |>
    vapply("[[", "", "name")
  deckpaths <- paste0("https://raw.githubusercontent.com/", repo, "/main/decks/", deckfiles)
  decklabels <- gsub(".csv", "", deckfiles)
  titles <- vapply(deckpaths, get_title, character(1))
  decks <- paste0(titles, " (", decklabels, ")")
  if (!is.null(pattern)) {
    if (!is.character(pattern)) {
      pattern <- deparse(substitute(pattern))
    }
    deck_nums <- grep(pattern, decks, ignore.case = TRUE)
    decks <- decks[deck_nums]
    decklabels <- decklabels[deck_nums]
    titles <- titles[deck_nums]
    if (length(decks) == 0) {
      cli::cli_abort("No decks match the pattern entered. Try another pattern string.")
    }
  }
  if (!quiet) {
    cli::cli_text("Available flashcard decks")
    cli::cli_ol(decks)
  }
  invisible(list(decklabels = decklabels, decktitles = unname(titles), decks = decks))
}

#' Choose from available flashcard decks
#'
#' @description This function prints a list of flashcard decks to the console
#' and let's the user choose one of the decks. By default, the function searches the
#' [flashr_decks repo]("https://github.com/JeffreyRStevens/flashr_decks/"). But
#' other GitHub repos can be used.
#'
#' To narrow the results, include text in the `pattern` argument (for example,
#' `choose_deck(pattern = "r4ds")`).
#'
#' @param pattern String pattern to search in list of decks.
#' @param repo GitHub username and repo for deck repository in the format
#' of "username/repository". Default value is "JeffreyRStevens/flashr_decks".
#'
#' @return
#' Outputs a list of available built-in flashcard decks to the console, where
#' the user can choose one of the decks to generate flashcards.
#' @export
#'
#' @family functions for finding decks
#'
#' @examples
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
                        repo = "JeffreyRStevens/flashr_decks") {
    deck_list <- list_decks(pattern = pattern, repo = repo)
    choice <- readline(prompt = "Please enter the number for a deck or 0 to exit: ")
    choice <- as.numeric(gsub("\\.", "", choice))
    # print(choice)
    decks <- deck_list$decks
    decklabels <- deck_list$decklabels
    titles <- deck_list$decktitles
    if (choice %in% 1:length(decks)) {
      cli::cli_text("Creating {.field ", {unname(titles[choice])}, "} deck.")
      flashcard(decklabels[choice])
    } else if (identical(choice, 0)) {
      invisible()
    } else{
      cli::cli_abort("That response was not valid. Please rerun `choose_deck()` and enter a valid number for an available deck.")
    }
  }

get_title <- function(x) {
  data <- utils::read.csv(x)
  data$title[1]
}
