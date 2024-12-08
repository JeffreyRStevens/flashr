#' Extract code blocks from R Markdown or Quarto file
#'
#' @description
#' To extract code blocks, apply `extract_code()` to R Markdown or Quarto files
#' either locally or via a URL. This function returns a character vector where
#' each line of content from an R code block is an element of the vector. Code
#' block options are not returned---only the content of the block. Code blocks
#' from other languages/engines (e.g., Python) are not returned.
#'
#' @param file Character string of file name for text that includes code blocks.
#' Can be local file or URL.
#' @param empty Logical indicating whether to include empty lines (`""`) or
#' whether to remove empty lines (default is TRUE, which includes empty lines).
#' @param comments Logical indicating whether to include comment lines starting
#' with `#` or whether to remove comment lines (default is TRUE, which includes
#' comment lines).
#'
#' @return
#' Returns character vector of individual lines of code.
#'
#' @note
#' This function is adapted from one Yihui Xie posted at
#' <https://yihui.org/en/2023/01/func-call/>.
#'
#' @export
#'
#' @family functions for extracting code and functions
#'
#' @examples
#' extract_code("https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd")
extract_code <- function(file,
                         empty = TRUE,
                         comments = TRUE) {
  stopifnot(
    "'file' should be a character string with one element" =
      typeof(file) == "character" & length(file) == 1
  )
  res <- litedown::crack(file)
  code <- unlist(lapply(res, function(el) {
    if (el$options$engine == "r" && el$type == "code_chunk") el$source
  }))
  if (!empty) {
    code <- code[code != ""]
  }
  if (!comments) {
    code <- gsub(" ", "", code)
    code <- code[!grepl("^#", code)]
  }
  code
}


#' Extract function calls from character vector of R code
#'
#' @description
#' This function finds all of the R functions in a character vector of R code.
#' For R scripts, first use [`readLines()`] or [`readr::read_file()`] to import
#' the script into a character vector. For R Markdown or Quarto documents,
#' first use [`extract_code()`] to find all of the R code in code blocks. The
#' character vector can then be passed to `extract_functions()` to find all of
#' the functions. By default, all instances of functions are returned. To omit
#' duplicate functions, set `duplicates = FALSE`.
#'
#'
#' @param code Object that contains R code.
#' @param duplicates Logical indicating whether to include duplicates of
#' functions or whether to remove duplicates (default is TRUE, which includes
#' duplicates).
#'
#' @return
#' Returns character vector of function names without parentheses (e.g.,
#' it returns "library" rather than "library()") included in R code.
#'
#' @note
#' This function is adapted from one Yihui Xie posted at
#' <https://yihui.org/en/2023/01/func-call/>.
#'
#' @export
#'
#' @family functions for extracting code and functions
#'
#' @examples
#' extract_functions(extract_code(
#'   "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"
#' ))
extract_functions <- function(code,
                              duplicates = TRUE) {
  stopifnot("'code' should be a character vector" = typeof(code) == "character")
  d <- getParseData(x = parse(text = code, keep.source = TRUE))
  f <- d[d$token == "SYMBOL_FUNCTION_CALL", "text"]
  for (s in d[d$token == "SYMBOL", "text"]) {
    tryCatch(
      {
        ev <- eval(as.symbol(s), parent.frame())
        if (is.function(ev)) f <- c(f, s)
      },
      error = function(e) NULL
    )
  }
  if (duplicates) {
    f
  } else {
    unique(f)
  }
}


#' Build data frame of functions for input to flashcard()
#'
#' @description
#' To create a data frame of functions that can be used to create a flashcard
#' deck, use `build_functions_df()`. This function calls [`extract_functions()`]
#' to find the functions if the `file` argument is specified. Otherwise, users
#' can pass a character vector of function names to the `fs` argument. Either
#' way, a title must be passed to `title` to create the data frame.
#'
#' Users can then either complete the _description_ column of the data frame
#' with their own descriptions or set the `desc` argument to TRUE to use
#' descriptions from
#' [flashr_decks](https://jeffreyrstevens.github.io/flashr_decks/functions.html).
#'
#'
#' @param file Character string of file name for text that includes code blocks.
#' Can be local file or URL.
#' @param fs If not using a file, character vector of functions
#' \[do not include `()`\].
#' @param title Character string of title for flashcard deck (required)
#' @param desc Logical for whether to search for descriptions from
#' flashr_decks  (default is TRUE, which includes descriptions from
#' flashr_decks).
#' @param omit Logical for whether to omit terms that have no descriptions from
#' flashr_decks (default is TRUE, which omits terms with no descriptions).
#'
#' @return
#' Data frame suitable to include in `flashcard()`.
#'
#' @export
#'
#' @family functions for extracting code and functions
#'
#' @examples
#' build_functions_df(fs = c("apple", "apply", "+"), title = "Test")
build_functions_df <- function(file = NULL,
                               fs = NULL,
                               title,
                               desc = TRUE,
                               omit = TRUE) {
  # Validate arguments
  stopifnot(
    "Needs argument for either file or fs but not both" =
      (is.null(file) & !is.null(fs)) | (!is.null(file) & is.null(fs))
  )
  if (!is.null(file)) {
    stopifnot(
      "'file' should be a character string with one element" =
        typeof(file) == "character" & length(file) == 1
    )
  }
  if (!is.null(fs)) stopifnot("'fs' should be a character vector" = typeof(fs) == "character")
  stopifnot("'title' should be a character vector" = typeof(title) == "character")
  stopifnot("'desc' should be a logical" = typeof(desc) == "logical")

  # Extract functions from files
  if (!is.null(file)) fs <- extract_functions(extract_code(file))

  # Create vector of operators
  operators_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/inst/extdata/operators.csv"
  fail_gracefully(operators_csv)
  operators <- utils::read.csv(operators_csv)$term

  # Create vector unique functions with appropriate ()
  unique_functions <- sort(unique(fs))
  functions <- ifelse(unique_functions %in% operators, unique_functions, paste0(unique_functions, "()"))

  # Initiate descrips and pkgs
  descrips <- pkgs <- NA_character_

  # Pull descriptions and packages from flashr_decks function CSV
  if (desc) {
    functions_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr_decks/refs/heads/main/data/functions.csv"
    fail_gracefully(functions_csv)
    all_functions <- utils::read.csv(functions_csv)
    for (i in seq_along(functions)) {
      descrips[i] <- ifelse(functions[i] %in% all_functions$term,
        all_functions[all_functions$term == functions[i], ]$description,
        NA_character_
      )
      pkgs[i] <- ifelse(functions[i] %in% all_functions$term,
        all_functions[all_functions$term == functions[i], ]$package,
        NA_character_
      )
    }
  }
  df <- data.frame(term = functions, description = descrips, package = pkgs, title = title)
  if (omit) {
    df <- df[!is.na(df$desc), ]
  }
  df
}
