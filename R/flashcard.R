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
#' @param fontsize Base font size for presentation. Acceptable values include
#' "default" (500%), "large" (700%), and "small" (300%). Custom values can be
#' set as percentages (e.g., "250%").
#' @param fontcolor Font color for non-link text.  Can be R color name, HTML
#' color name, or hex code.
#' @param linkcolor Font color for link text.  Can be R color name, HTML
#' color name, or hex code.
#' @param use_browser Logical indicating whether to show the presentation in the
#' RStudio viewer when available (FALSE) or the system's default browser (TRUE)
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
#' # Display custom CSV file of terms and descriptions.
#' # If package information is not included, set `package = FALSE`.
#' flashcard(file = "data/operators.csv", package = FALSE)
#' }
flashcard <- function(x,
                      termsfirst = TRUE,
                      package = TRUE,
                      theme = "moon",
                      file = NULL,
                      fontsize = "default",
                      fontcolor = NULL,
                      linkcolor = NULL,
                      use_browser = FALSE) {

  # Validate deck
  deck <- validate_deck(x, package = package)

  # Assign deck title and deckname
  title <- attr(deck, "title")
  deckname <- attr(deck, "deckname")
  package <- attr(deck, "package")

  # Shuffle order of items
  items <- deck[sample(nrow(deck)), ]

  # Determine fontsize
  if (!grepl("%", fontsize)) {
    if (fontsize == "default") {
      fontsize <- "500%"
    } else if (fontsize == "large") {
      fontsize <- "700%"
    } else if (fontsize == "small") {
      fontsize <- "300%"
    } else {
      cli::cli_abort("The {.code fontsize} value is invalid. Please specify a valid font size.")
    }
  }

  # Check font and colors
  if (!is.null(fontcolor)) {
    if (!is_color(fontcolor)) {
      cli::cli_abort("The {.code fontcolor} {fontcolor} is not a valid color.")
    }
  }

  if (!is.null(linkcolor)) {
    if (!is_color(linkcolor)) {
      cli::cli_abort("The {.code linkcolor} {linkcolor} is not a valid color.")
    }
  }

  # Create YAML header for reveal.js presentation
  text <- c(
    "---",
    paste0('title: "', title, '"'),
    "output:",
    "  revealjs::revealjs_presentation:",
    paste0("    theme: ", theme),
    "    center: true",
    "---",
    "<style>"
  )
  if (is.null(fontcolor)) {
    font_style <- c(
      ".reveal {",
      paste0("font-size: ", fontsize, ";"),
      "}"
    )
  } else {
    font_style <- c(
      ".reveal {",
      paste0("font-size: ", fontsize, ";"),
      paste0("color: ", fontcolor, ";"),
      "}"
    )
  }

  if (is.null(linkcolor)) {
    link_style <- ""
  } else {
    link_style <- c(
      ".reveal a {",
      paste0("color: ", linkcolor, ";"),
      "}"
    )
  }
  text <- c(text, font_style, link_style, "</style>")


  # Create slides for each item
  for (i in seq_len(nrow(items))) {

    # Create slide components
    term <- paste0("`", items$term[i], "`")
    # Add URL if included in deck
    if ("url" %in% names(deck)) {
      if (!is.na(items$url[i])) {
        term <- paste0("[", term, "](", items$url[i], ")")
      }
    }
    description <- items$description[i]
    if (package & "package" %in% names(deck)) {
      if (!is.na(items$package[i])) {
        pack <- paste0("{", items$package[i], "}")
      }
    } else {
      pack <- ""
    }

    # Create slide from components
    if (termsfirst) {
      item <- c("##", term, "", pack, "", "##", description, "", "##", "")
    } else {
      item <- c("##", description, "", "##", term, "", pack, "", "##", "")
    }

    # Add slide to deck
    text <- c(text, item)
  }

  # Create R Markdown and HTML file names in temporary directory
  dir <- tempfile()
  dir.create(dir)
  rmdfile <- file.path(dir, paste0(deckname, ".Rmd"))
  htmlfile <- file.path(dir, paste0(deckname, ".html"))

  # Write to R Markdown file and render HTML file
  writeLines(text = text, con = rmdfile)
  revealjs::revealjs_presentation()
  rmarkdown::render(input = rmdfile, quiet = TRUE)

  # Save HTML file when requested
  if (!is.null(file)) {
    if (identical(tolower(tools::file_ext(file)), "html")) {
      file.copy(from = htmlfile, to = file, overwrite = TRUE)
    } else {
      cli::cli_abort("Output files must be HTML or html.")
    }
  }

  # Open HTML file in viewer
  if (!testthat::is_testing()) {
    viewer <- getOption("viewer")
    if (!is.null(viewer) && !use_browser) {
      viewer(htmlfile)
    } else {
      utils::browseURL(htmlfile)
    }
  }
}

validate_deck <- function(x, package = package) {
  # Convert all deck objects to strings
  valid_decks <- list_decks(quiet = TRUE)
  if (is.character(x)) {
    input <- x
  } else {
    input <- deparse(substitute(x))
  }

  # Validate input
  if (length(input) > 1) {
    cli::cli_abort("Input is a vector rather than available deck or CSV file.")
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
      cli::cli_alert_info(
        "No {.field title} column, so using filename for title."
      )
    }
  } else if (input %in% valid_decks$decklabels) { # if input is in valid decks
    # Get deck and deckname
    deck <- utils::read.csv(paste0("https://raw.githubusercontent.com/JeffreyRStevens/flashr_decks/main/decks/", input, ".csv"),
      na.strings = ""
    )
    deckname <- input
    title <- deck$title[1]
  } else { # if input is not CSV or valid deck
    cli::cli_abort(
      "This deck is not recognized as a available deck or a valid CSV file."
    )
  }

  # Check if package column is present if package = TRUE
  if (package && !"package" %in% names(deck)) {
    cli::cli_alert_info("This deck does not include a {.field package} column. Setting {.code package = FALSE}.")
    package <- FALSE
  }

  # Assign title and deckname and invisbily return output
  attr(deck, "title") <- title
  attr(deck, "deckname") <- deckname
  attr(deck, "package") <- package
  invisible(deck)
}

is_color <- function(x) {
  web_colors <- c("Pink", "LightPink", "HotPink", "DeepPink", "PaleVioletRed", "MediumVioletRed", "LightSalmon", "Salmon", "DarkSalmon", "LightCoral", "IndianRed", "Crimson", "FireBrick", "DarkRed", "Red", "OrangeRed", "Tomato", "Coral", "DarkOrange", "Orange", "Yellow", "Yellow", "LightYellow", "LemonChiffon", "LightGoldenrodYellow", "PapayaWhip", "Moccasin", "PeachPuff", "PaleGoldenrod", "Khaki", "DarkKhaki", "Gold", "Cornsilk", "BlanchedAlmond", "Bisque", "NavajoWhite", "Wheat", "BurlyWood", "Tan", "RosyBrown", "SandyBrown", "Goldenrod", "DarkGoldenrod", "Peru", "Chocolate", "SaddleBrown", "Sienna", "Brown", "Maroon", "DarkOliveGreen", "Olive", "OliveDrab", "YellowGreen", "LimeGreen", "Lime", "LawnGreen", "Chartreuse", "GreenYellow", "SpringGreen", "MediumSpringGreen", "LightGreen", "PaleGreen", "DarkSeaGreen", "MediumSeaGreen", "SeaGreen", "ForestGreen", "Green", "DarkGreen", "MediumAquamarine", "Aqua", "Cyan", "LightCyan", "PaleTurquoise", "Aquamarine", "Turquoise", "MediumTurquoise", "DarkTurquoise", "LightSeaGreen", "CadetBlue", "DarkCyan", "Teal", "LightSteelBlue", "PowderBlue", "LightBlue", "SkyBlue", "LightSkyBlue", "DeepSkyBlue", "DodgerBlue", "CornflowerBlue", "SteelBlue", "RoyalBlue", "Blue", "MediumBlue", "DarkBlue", "Navy", "MidnightBlue", "Lavender", "Thistle", "Plum", "Violet", "Orchid", "Fuchsia", "Magenta", "MediumOrchid", "MediumPurple", "BlueViolet", "DarkViolet", "DarkOrchid", "DarkMagenta", "Purple", "Indigo", "DarkSlateBlue", "RebeccaPurple", "SlateBlue", "MediumSlateBlue", "White", "Snow", "Honeydew", "MintCream", "Azure", "AliceBlue", "GhostWhite", "WhiteSmoke", "Seashell", "Beige", "OldLace", "FloralWhite", "Ivory", "AntiqueWhite", "Linen", "LavenderBlush", "MistyRose", "Gainsboro", "LightGrey", "Silver", "DarkGray", "Gray", "DimGray", "LightSlateGray", "SlateGray", "DarkSlateGray", "Black")
  all_colors <- c(web_colors, tolower(web_colors), grDevices::colors())
  return(x %in% all_colors | grepl("^#(\\d|[a-f]){6,8}$",
    x,
    ignore.case = TRUE
  ))
}
