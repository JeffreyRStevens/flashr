#' Create flashcards
#'
#' @description
#' The `flashcard()` function generates a set of flashcards with randomly
#' ordered pairs of terms and descriptions. The function outputs reveal.js
#' presentation as an HTML file. If running in RStudio, the flashcards are
#' output to the viewer. Otherwise, they are output to a web browser.
#'
#' @param deck Name of pre-existing flashcard deck to generate
#' @param file Path and name of CSV file containing terms and descriptions
#' @param termsfirst Logical indicating whether to show terms first (TRUE) or
#' descriptions first (FALSE)
#' @param package Logical indicating whether to include package name in term
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
flashcard <- function(deck,
                      file = NULL,
                      termsfirst = TRUE,
                      package = TRUE) {

  # Check if using pre-existing deck or file
  if (is.null(file)) {
    # Get deck name and title from object
    deckname <- deparse(substitute(deck))
    title <- attr(deck, "title")
  } else {
    # Import external file
    deck <- utils::read.csv(file)
    # Get deck name from file name
    deckname <- gsub(".csv", "", basename(file))
    # Get title from file or use file name
    if ("name" %in% names(deck)) {
      title <- deck$name[1]
    } else {
      title <- deckname
    }
  }

  # Check if package column is present if package = TRUE
  if (package & !"package" %in% names(deck)) {
    stop("This deck does not include a 'package' column. Choose another deck or set `package = FALSE`.")
  }

  # Shuffle order of items
  items <- deck[sample(nrow(deck)), ]

  # Create YAML header for reveal.js presentation
  text <- c("---", paste0('title: "', title, '"'), "output: revealjs::revealjs_presentation", "---")

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
  rmarkdown::render(rmdfile)

  # Open HTML file in viewer
  viewer <- getOption("viewer")
  # viewer(htmlfile)
  if (!is.null(viewer)) {
    viewer(htmlfile)
  } else {
    utils::browseURL(htmlfile)
  }
}
