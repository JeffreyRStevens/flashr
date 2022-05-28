
#' List available built-in flashcard decks
#'
#' @param quiet Logical to prevent list information from printing to console.
#'
#' @return
#' Outputs a list of available built-in flashcard decks to the console.
#' @export
#'
#' @examples
#' list_decks()
list_decks <- function(quiet = FALSE) {
  available_decks <- get_decks()
  if (!quiet) {
    cli::cli_text("Available flashcard decks")
    cli::cli_ol(available_decks$decks)
  }
}

#' Choose from available flashcard decks
#'
#' @param x Name of deck to directly generate flashcards (allows choose_deck()
#' to act as wrapper for flashcard()).
#'
#' @return
#' Outputs a list of available built-in flashcard decks to the console, where
#' the user can choose of the decks to generate flashcards.
#' @export
#'
#' @examples
choose_deck <- function(x = NULL) {
  if (!is.null(x)) {
    flashcard(x)
  } else {
    list_decks()
    choice <- readline(prompt = "Please enter the number for a deck: ")
    choice <- as.numeric(gsub("\\.", "", choice))
    print(choice)
    if (choice %in% 1:length(decks)) {
      cli::cli_text("Creating {.field ", {unname(titles[choice])}, "} deck.")
      deck <- eval(parse(text = decklabels[choice]))
      flashcard(decklabels[choice])
    } else {
      cli::cli_abort("That response was not valid. Please rerun `choose_deck()` and enter a valid number for an available deck.")
    }
  }
}

get_decks <- function() {
  deckfiles <- list.files(path = ("inst/extdata/"))
  deckpaths <- paste0("inst/extdata/", deckfiles)
  decklabels <- gsub(".csv", "", deckfiles)
  titles <- vapply(deckpaths, get_title, character(1))
  decks <- paste0(titles, " (", decklabels, ")")
  invisible(list(decklabels = decklabels, decktitles = unname(titles), decks = decks))
}

get_title <- function(x) {
  data <- utils::read.csv(x)
  data$title[1]
}
