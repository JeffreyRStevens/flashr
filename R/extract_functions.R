

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
  x = xfun::read_utf8(file)
  on.exit(knitr::knit_code$restore(), add = TRUE)
  res = knitr:::split_file(x, patterns = knitr::all_patterns$md)
  unlist(lapply(res, function(el) {
    if (is.null(label <- el$params$label)) el$code else {
      if (is.null(lang <- el$params$engine) || tolower(lang) == 'r')
        knitr::knit_code$get(label)
    }
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
#' extract_functions(extract_code("https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd"))
extract_functions <- function(code) {
  d = getParseData(parse(text = code))
  f = d[d$token == 'SYMBOL_FUNCTION_CALL', 'text']
  for (s in d[d$token == 'SYMBOL', 'text']) {
    tryCatch({
      ev = eval(as.symbol(s), parent.frame())
      if (is.function(ev)) f = c(f, s)
    }, error = function(e) NULL)
  }
  f
}

