#' Create flashcards
#'
#' @description
#' The `flashcard()` function generates a set of flashcards with randomly
#' ordered pairs of terms and descriptions. The function outputs reveal.js
#' presentation as an HTML file. If running in RStudio, the flashcards are
#' output to the viewer. Otherwise, they are output to a web browser.
#'
#' @param x Name of pre-existing flashcard deck or path and name of CSV file
#' containing terms and descriptions
#' @param termsfirst Logical indicating whether to show terms first (TRUE) or
#' descriptions first (FALSE)
#' @param package Logical indicating whether to include package name in term
#' @param theme Name of reveal.js theme to use for flashcards
#' @param file Path and file name used to save flashcard deck locally (must
#' save as HTML)
#'
#' @return
#' An HTML file of terms and descriptions rendered in the RStudio viewer or
#' web browser.
#' @export
#'
#' @examples
#' \dontrun{
#' # Display terms then descriptions
#' flashcard(data_types)
#'
#' # Display descriptions then terms
#' flashcard(data_types, termsfirst = FALSE)
#'
#' # Display custom CSV file of terms and descriptions. If package information
#' is not included, set `package = FALSE`.
#' flashcard(file = "data/operators.csv", package = FALSE)
#' }
flashcard <- function(x,
                      termsfirst = TRUE,
                      package = TRUE,
                      theme = "moon",
                      file = NULL) {

  # Convert all deck objects to strings
  if (is.character(x)) {
    x <- x
  } else {
    x <- deparse(substitute(x))
  }

  # Validate deck
  deck <- validate_deck(x, package = package)

  # Assign deck title and deckname
  title <- attr(deck, "title")
  deckname <- attr(deck, "deckname")

  # Shuffle order of items
  items <- deck[sample(nrow(deck)), ]

  # Create YAML header for reveal.js presentation
  text <- c("---", paste0('title: "', title, '"'), "output:", "  revealjs::revealjs_presentation:", paste0("    theme: ", theme), "    center: true", paste0('    footer: "', title, '"'), "---")

  # Create slides for each item
  for (i in 1:nrow(items)) {
    if (termsfirst) {
      if (package) {
        item <- c("##", "", "##", paste0("`", items$term[i], "`"), "", paste0("{", items$package[i], "}"), "", "##", items$description[i], "")
      } else {
        item <- c("##", "", "##", paste0("`", items$term[i], "`"), "", "##", items$description[i], "")
      }
    } else {
      if(package) {
        item <- c("##", "", "##", items$description[i], "", "##", paste0("`", items$term[i], "`"), "", paste0("{", items$package[i], "}"), "")
      } else {
        item <- c("##", "", "##", items$description[i], "", "##", paste0("`", items$term[i], "`"), "")
      }
    }
    text <- c(text, item)
  }

  # Create R Markdown and HTML file names in temporary directory
  dir <- tempfile()
  dir.create(dir)
  rmdfile <- file.path(dir, paste0(deckname, ".Rmd"))
  htmlfile <- file.path(dir, paste0(deckname, ".html"))

  # Write to R Markdown file and render HTML file
  writeLines(text = text, con = rmdfile)
  rmarkdown::render(rmdfile, quiet = TRUE)

  # Open HTML file in viewer
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(htmlfile)
  } else {
    utils::browseURL(htmlfile)
  }
}

validate_deck <- function(x, package = package) {
  # Convert all deck objects to strings
  valid_decks <- get_decks()
  if (is.character(x)) {
    input <- x
  } else {
    input <- deparse(substitute(x))
  }

  # Validate input
  if (length(input) > 1) {
    cli::cli_abort("Input is a vector rather than built-in deck or CSV file.")
  }

  if (grepl(".csv", input)) { # if input is CSV file
    # Get deck and deckname
    deck <- utils::read.csv(input)
    deckname <- gsub(".csv", "", basename(x))

    # Get title from file or use file name
    if ("title" %in% names(deck)) {
      title <- deck$title[1]
    } else {
      title <- deckname
      cli::cli_alert_info("No {.field title} column, so using filename for title.")
    }

  } else if (input %in% valid_decks$decklabels) { # if input is found in valid decks
    # Get deck and deckname
    deck <- eval(parse(text = input))
    deckname <- input

    # Get title from data frame or use object name
    title <- attr(deck, "title")

  } else { # if input is not CSV or valid deck
    cli::cli_abort("This deck is not recognized as a built-in deck or a valid CSV file.")
  }

  # Check if package column is present if package = TRUE
  if (package & !"package" %in% names(deck)) {
    cli::cli_abort("This deck does not include a {.field package} column. Choose another deck or set {.code package = FALSE}.")
  }

  # Assign title and deckname and invisbily return output
  attr(deck, "title") <- title
  attr(deck, "deckname") <- deckname
  invisible(deck)
}

