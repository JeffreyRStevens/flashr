

#' Extract code chunks from R Markdown or Quarto file
#'
#' @param file Character string of file name. Can be local file or URL.
#'
#' @return
#' Returns character vector of individual lines of code.
#'
#' @export
#'
#' @examples
#' extract_code("https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd")
extract_code <- function(file) {
  x <- xfun::read_utf8(file)
  res <- litedown::crack(x)
  unlist(lapply(res, function(el) {
    if (el$type == "code_chunk") el$source
  }))
}


#' Extract function calls from character vector of R code
#'
#' @param code Object that contains R code
#'
#' @return
#' Returns character vector of function names without parentheses (e.g.,
#' it returns "library" rather than "library()") included in R code.
#'
#' @export
#'
#' @examples
#' extract_functions(extract_code(
#'   "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"))
extract_functions <- function(code) {
  d <- getParseData(x = parse(text = code, keep.source = TRUE))
  f <- d[d$token == 'SYMBOL_FUNCTION_CALL', 'text']
  for (s in d[d$token == 'SYMBOL', 'text']) {
    tryCatch({
      ev <- eval(as.symbol(s), parent.frame())
      if (is.function(ev)) f = c(f, s)
    }, error = function(e) NULL)
  }
  f
}



build_functions_file <- function(fs, title, desc = TRUE) {
  # Validate arguments
  stopifnot("fs should be a character vector" = typeof(fs) == "character")
  stopifnot("title should be a character vector" = typeof(title) == "character")
  stopifnot("desc should be a logical" = typeof(desc) == "logical")

  # Create vector of operators
  operators_csv <- "https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/inst/extdata/operators.csv"
  fail_gracefully(operators_csv)
  operators <- utils::read.csv(operators_csv)$term

  # Create vector unique functions with appropriate ()
  unique_functions <- unique(fs)
  functions <- ifelse(unique_functions %in% operators, fs, paste0(fs, "()"))

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
                            NA_character_)
      pkgs[i] <- ifelse(functions[i] %in% all_functions$term,
                        all_functions[all_functions$term == functions[i], ]$package,
                        NA_character_)
    }
  }
  data.frame(term = functions, description = descrips, package = pkgs, title = title)
}

