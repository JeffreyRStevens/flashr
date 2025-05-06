#' Create flashcards
#'
#' @description
#' The `flashcard()` function generates a set of flashcards with randomly
#' ordered pairs of terms and descriptions from built-in flashcard decks.
#' The function outputs reveal.js presentation as an HTML file.
#' If running in RStudio, the flashcards are output to the viewer.
#' Otherwise, they are output to a web browser.
#'
#' @param x Name of pre-existing flashcard deck or path and name of CSV file
#' containing terms and descriptions
#' @param termsfirst Logical indicating whether to show terms first (TRUE) or
#' descriptions first (FALSE)
#' @param package Logical indicating whether to include package name in term
#' @param theme Name of reveal.js theme to use for flashcards
#' @param file Path and file name used to save flashcard deck locally (must
#' save as HTML)
#' @param random Logical indicating whether to randomize order of terms (TRUE)
#' or use order from data frame
#' @param fontsize Base font size for presentation. Acceptable values include
#' "default" (500%), "large" (700%), and "small" (300%). Custom values can be
#' set as percentages (e.g., "250%").
#' @param fontcolor Font color for non-link text.  Can be R color name, HTML
#' color name, or hex code.
#' @param linkcolor Font color for link text.  Can be R color name, HTML
#' color name, or hex code.
#' @param use_browser Logical indicating whether to show the presentation in the
#' RStudio viewer when available (FALSE) or the system's default browser (TRUE)
#' @param omit_na Logical indicating whether to omit terms that have no
#' descriptions from the deck (default is TRUE, which omits terms with no
#' descriptions)
#'
#' @return
#' An HTML file of terms and descriptions rendered in the RStudio viewer or
#' web browser.
#'
#' @export
#'
#' @note This function **requires internet connectivity** to use existing decks.
#' An internet connection is not required if you supply a CSV file. However,
#' without an internect connection, themes other than _black_, _white_, and
#'  _serif_, may not render properly, as they require access to Google Fonts.
#'
#' @family functions for creating decks
#'
#' @examplesIf interactive()
#' \donttest{
#' # Display terms then descriptions
#' flashcard("data_types")
#'
#' # Display descriptions then terms
#' flashcard("data_types", termsfirst = FALSE)
#'
#' # Display terms without package information
#' flashcard("data_types", package = FALSE)
#' }
flashcard <- function(x,
                      termsfirst = TRUE,
                      package = TRUE,
                      theme = "moon",
                      file = NULL,
                      random = TRUE,
                      fontsize = "default",
                      fontcolor = NULL,
                      linkcolor = NULL,
                      use_browser = FALSE,
                      omit_na = TRUE) {
  # Check arguments
  check_logical("package", package)
  check_logical("omit_na", omit_na)

  # Validate deck
  deck <- validate_deck(x, pkg = package, omit_na = omit_na)

  # Assign deck title and deckname
  title <- attr(deck, "title")
  package <- attr(deck, "package")

  build_deck(deck,
    title = title,
    termsfirst = termsfirst,
    package = package,
    theme = theme,
    file = file,
    random = random,
    fontsize = fontsize,
    fontcolor = fontcolor,
    linkcolor = linkcolor,
    use_browser = use_browser
  )
}

#' Create deck from vector of functions
#'
#' @description
#' The `create_deck()` function generates a set of flashcards with randomly
#' ordered pairs of terms and descriptions from a vector of functions provided
#' by the user. The function outputs reveal.js presentation as an HTML file.
#' If running in RStudio, the flashcards are output to the viewer.
#' Otherwise, they are output to a web browser.
#'
#' @param x Name of pre-existing flashcard deck or path and name of CSV file
#' containing terms and descriptions
#' @param title Title provided for flashcard deck. Defaults to "Custom deck" if
#' not provided.
#' @param termsfirst Logical indicating whether to show terms first (TRUE) or
#' descriptions first (FALSE)
#' @param package Logical indicating whether to include package name in term
#' @param theme Name of reveal.js theme to use for flashcards
#' @param file Path and file name used to save flashcard deck locally (must
#' save as HTML)
#' @param random Logical indicating whether to randomize order of terms (TRUE)
#' or use order from data frame
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
#'
#' @export
#'
#' @family functions for creating decks
#'
#' @examplesIf interactive()
#' \donttest{
#' # Display terms then descriptions
#' my_functions <- c("as_tibble()", "bind_rows()", "c()")
#' create_deck(x = my_functions)
#'
#' # Customize the title
#' create_deck(x = my_functions, title = "My deck")
#'
#' # Save the HTML version of the flashcard deck locally
#' create_deck(x = my_functions, title = "My deck", file = "my_deck.html")
#' }
create_deck <- function(x,
                        title = NULL,
                        termsfirst = TRUE,
                        package = TRUE,
                        theme = "moon",
                        file = NULL,
                        random = TRUE,
                        fontsize = "default",
                        fontcolor = NULL,
                        linkcolor = NULL,
                        use_browser = FALSE) {
  # Assign deck title and deckname
  deck <- select_terms(x)

  build_deck(deck,
    title = title,
    termsfirst = termsfirst,
    package = package,
    theme = theme,
    file = file,
    random = random,
    fontsize = fontsize,
    fontcolor = fontcolor,
    linkcolor = linkcolor,
    use_browser = use_browser
  )
}

validate_deck <- function(x,
                          pkg = package,
                          omit_na = omit_na) {
  # Convert all deck objects to strings
  if (length(x) == 1) {
    if (!grepl(".csv", x, ignore.case = TRUE)) {
      valid_decks <- list_decks(quiet = TRUE)
    }
  }
  if (is.character(x)) {
    input <- x
  } else {
    input <- deparse(substitute(x))
  }

  if (length(x) == 1) {
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
        "This deck is not recognized as a available deck or a valid data frame or CSV file."
      )
    }
  } else if (inherits(x, "data.frame")) { # if input is a data frame
    deck <- x
    deckname <- deparse(substitute(x))
    if (!"term" %in% names(deck)) {
      cli::cli_abort(
        "This data frame does not have term column."
      )
    } else if (!"description" %in% names(deck)) {
      cli::cli_abort(
        "This data frame does not have description column."
      )
    } else {
      if ("title" %in% names(deck)) {
        title <- deck$title[1]
      } else {
        title <- deckname
        cli::cli_alert_info(
          "No title column, so using {deckname} for title."
        )
      }
    }
  } else { # if input is not CSV or valid deck
    cli::cli_abort(
      "This deck is not recognized as a available deck or a valid data frame or CSV file."
    )
  }
  # Check if package column is present if package = TRUE
  if (pkg && !"package" %in% names(deck)) {
    cli::cli_alert_info("This deck does not include a {.field package} column. Setting {.code package = FALSE}.")
    package <- FALSE
  } else if (pkg) {
    package <- TRUE
  } else {
    package <- FALSE
  }

  # Remove missing descriptions
  if (omit_na) {
    deck <- deck[!is.na(deck$description), ]
    deck <- deck[deck$description != "NA", ]
  }

  # Assign title and deckname and invisbily return output
  attr(deck, "title") <- title
  attr(deck, "deckname") <- deckname
  attr(deck, "package") <- package
  invisible(deck)
}

build_deck <- function(deck,
                       title = title,
                       termsfirst = termsfirst,
                       package = package,
                       theme = theme,
                       file = file,
                       random = random,
                       fontsize = fontsize,
                       fontcolor = fontcolor,
                       linkcolor = linkcolor,
                       use_browser = use_browser) {
  # Check arguments
  check_character("title", title, nullok = TRUE)
  check_logical("termsfirst", termsfirst)
  check_character("theme", theme, c(
    "default", "black", "white", "league", "beige",
    "night", "serif", "simple", "solarized",
    "moon", "sky", "blood"
  ))
  check_character("file", file, nullok = TRUE)
  check_logical("random", random)
  check_character("fontsize", fontsize)
  check_character("fontcolor", fontcolor, nullok = TRUE)
  check_character("linkcolor", linkcolor, nullok = TRUE)
  check_logical("use_browser", use_browser)

  # Shuffle order of items
  if (random) {
    items <- deck[sample(nrow(deck)), ]
  } else {
    items <- deck
  }

  # Create title and deckname
  if (is.null(title)) {
    title <- "Custom deck"
  }
  deckname <- tolower(gsub(" ", "_", title))

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
    if (grepl("\\$(.*?)\\$", items$term[i])) {
      term <- items$term[i]
    } else {
      term <- paste0("`", items$term[i], "`")
    }
    # Add URL if included in deck
    if ("url" %in% names(deck)) {
      if (!is.na(items$url[i])) {
        term <- paste0("[", term, "](", items$url[i], ")")
      }
    }
    description <- items$description[i]
    if (package && "package" %in% names(deck)) {
      if (!is.na(items$package[i])) {
        pack <- paste0("{", items$package[i], "}")
      } else {
        pack <- ""
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
  text <- c(text, "##", "The end!", "")

  # Create R Markdown and HTML file names in temporary directory
  dir <- tempfile()
  dir.create(dir)
  rmdfile <- file.path(dir, paste0(deckname, ".Rmd"))
  htmlfile <- file.path(dir, paste0(deckname, ".html"))

  # Write to R Markdown file and render HTML file
  writeLines(text = text, con = rmdfile)
  revealjs::revealjs_presentation()
  rmarkdown::render(input = rmdfile, quiet = TRUE)

  # Save HTML or Rmd file when requested
  if (!is.null(file)) {
    if (identical(tolower(tools::file_ext(file)), "html")) {
      file.copy(from = htmlfile, to = file, overwrite = TRUE)
    } else if (identical(tolower(tools::file_ext(file)), "rmd")) {
      file.copy(from = rmdfile, to = file, overwrite = TRUE)
    } else {
      cli::cli_abort("Output files must be HTML or Rmd.")
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

select_terms <- function(x) {
  functions_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr_decks/refs/heads/main/data/functions.csv"
  fail_gracefully(functions_csv)
  all_functions <- utils::read.csv(functions_csv)
  functions <- all_functions$term
  operators <- subset(all_functions, !grepl("\\w::\\w", all_functions$function_name))
  operators <- operators$term

  # Check if all functions are operators or include ()
  if (!all(grepl("\\(\\)", x) | x %in% operators)) {
    wrong_functions <- x[which(!grepl("\\(\\)", x) & !x %in% operators)]
    cli::cli_abort(c(
      "The following {cli::qty(wrong_functions)} entr{?y/ies} do{?es/} not include `()` (for example, \"library()\"). Please append () at the end of all functions (except operators).",
      "{.field {wrong_functions}}"
    ))
  }

  # Check if all functions are in functions
  if (!all(x %in% functions)) {
    missing_functions <- x[which(!x %in% functions)]
    cli::cli_warn(c(
      "The following {cli::qty(missing_functions)} entr{?y/ies} w{?as/ere} not found in the list of functions. The deck is being created without {?it/them}. Correct or remove {?it/them} to stop this message.",
      "{.field {missing_functions}}"
    ))
  }

  df <- data.frame(term = x)
  deck <- all_functions[all_functions$term %in% df$term, , drop = FALSE]
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
