#' Create flashcards
#'
#' @description
#' The `flashcard()` function generates a set of flashcards with randomly
#' ordered pairs of terms and descriptions. The function outputs reveal.js
#' presentation as an HTML file. If running in RStudio, the flashcards are
#' output to the viewer. Otherwise, they are output to a web browser.
#'
#' @param deck Name of pre-existing flashcard deck to generate
#' @param termsfirst Logical indicating whether to show terms first (TRUE) or
#' descriptions first (FALSE)
#'
#' @return
#' An HTML file of terms and descriptions rendered in the RStudio viewer or
#' web browser.
#' @export
#'
#' @examples
#' \dontrun{
#' flashcard(data_type)
#' }
flashcard <- function(deck,
                      termsfirst = TRUE) {

  # # Create file name and path
  # filename <- paste0(deck, ".csv")
  # path <- "data/"
  # filepath <- paste0(path, filename)
  #
  # Import data file
  # data <- read.csv(here::here(filepath))

  # Get deck name and title
  deckname <- deparse(substitute(deck))
  title <- deck$name[1]

  # Shuffle order of items
  items <- deck[sample(nrow(deck)), 1:2]

  # Create YAML header for reveal.js presentation
  text <- c("---", paste0('title: "', title, '"'), "output: revealjs::revealjs_presentation", "---")

  # Create slides for each item
  for (i in 1:nrow(items)) {
    if (termsfirst) {
      item <- c("##", "", "##", paste0("`", items$term[i], "`"), "", "##", items$description[i], "")
    } else {
      item <- c("##", "", "##", items$description[i], "", "##", paste0("`", items$term[i], "`"), "")
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
