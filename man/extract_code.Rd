% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_functions.R
\name{extract_code}
\alias{extract_code}
\title{Extract code chunks from R Markdown or Quarto file}
\usage{
extract_code(file)
}
\arguments{
\item{file}{Character string of file name for text that includes code chunks.
Can be local file or URL.}
}
\value{
Returns character vector of individual lines of code.
}
\description{
Extract code chunks from R Markdown or Quarto file
}
\note{
This function is adapted from one Yihui Xie posted at
https://yihui.org/en/2023/01/func-call/.
}
\examples{
extract_code("https://raw.githubusercontent.com/JeffreyRStevens/flashr/refs/heads/main/README.Rmd")
}
