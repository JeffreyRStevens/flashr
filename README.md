
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flashr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/JeffreyRStevens/flashr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JeffreyRStevens/flashr?branch=main)
<!-- badges: end -->

Learning all of the functions needed to become proficient in R is a
substantial undertaking. Flashcards are a great way to learn the syntax
of computer languages (Hermans 2021). The goal of `{flashr}` is to
provide a quick way to view decks of flashcards.

`{flashr}` includes existing flashcard decks, including functions and
arguments from [R for Data Science (first
edition)](https://r4ds.had.co.nz/). The aim is to include decks from [R
for Data Science (second edition)](https://r4ds.hadley.nz) when it is
complete.

In addition to built in decks, you can easily create your own decks
using CSV files. This allows users to customize existing decks or create
completely new decks. Also, while geared toward learning R, this package
can be used to build decks for anything—not just computer syntax!

## Installation

You can install the development version of `{flashr}` like so:

``` r
# install.packages("devtools")
devtools::install_github("JeffreyRStevens/flashr")
```

## Example

To view, for example, the flashcard deck on data types:

``` r
library(flashr)
flashcard(deck = data_types)
```

This randomizes the order of terms and give terms before descriptions.
If you would like to present descriptions before terms:

``` r
flashcard(deck = data_types, termsfirst = FALSE)
```

To build your own deck, simply save a CSV file with a `term` column and
a `description` column. You can also include a `package` column if you
want the package name included with the term and a `name` column if you
want to specify the title of the deck

``` r
my_deck <- read.csv("data/operators.csv")
head(my_deck)
#>   term            description package      name
#> 1    =    assignment operator    base Operators
#> 2   <-    assignment operator    base          
#> 3   |> pipe operator (base R)    base          
#> 4    +               addition    base          
#> 5    -            subtraction    base          
#> 6    *         multiplication    base
```

Then simply run the `flashcard()` function on your file or object. Note
that you need to use the `file` argument to specify external files and
the `deck` argument to specify existing R objects.

``` r
flashcard(file = "data/operators.csv")
flashcard(deck = my_deck)
```

## Citation

To cite `{flashr}`, use:

> Stevens, Jeffrey R. (2022). flashr: Creates flashcards of terms and
> definitions. (version 0.0.0.9000)
> <https://github.com/JeffreyRStevens/flashr>

## Contributing to this package

Contributions to `{flashr}` are most welcome! Feel free to check out
[open issues](https://github.com/JeffreyRStevens/flashr/issues) for
ideas. And [pull
requests](https://github.com/JeffreyRStevens/flashr/pulls) are
encouraged, but you may want to [raise an
issue](https://github.com/JeffreyRStevens/flashr/issues) or [contact the
maintainer](mailto:jeffrey.r.stevens@protonmail.com) first.

## References

Hermans, F. (2021). The Programmer’s Brain. Manning.
<https://www.manning.com/books/the-programmers-brain>
