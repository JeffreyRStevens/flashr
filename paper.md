---
title: 'flashr: An R package that creates flashcards'
tags:
  - R
  - rstats
  - open education
authors:
  - name: Jeffrey R. Stevens
    orcid: 0000-0003-2375-1360
    affiliation: 1
affiliations:
 - name: Department of Psychology, Center for Brain, Biology & Behavior, University of Nebraska-Lincoln
   index: 1
date: "2022-07-31"
bibliography: paper.bib
output:
  html_document:
    keep_md: yes
---

# Summary

Flashcards can be an effective way to learn new material, including vocabulary, spoken languages, and computer programming languages. 
The `flashr` R package creates decks of flashcards from sets of terms and descriptions built around learning R. 
This package draws from a repository of decks based on existing books and resources. 
New decks are easy to build, and users can create new decks either for their own use or to contribute to the repository. 
Moreover, the decks do not have to be about R or even programming languages. 
Users can develop their own decks for whatever material they would like to learn.


# Statement of need

Learning a new language---either spoken languages or computer programming languages---can require a lot of time to map new vocabulary words to their meaning. 
Repetition is critical to learning new vocabulary [@Laufer.etal.2005; @Larsen-Freeman.2012], and the repeated use of flashcards can provide an effective way to learn technical vocabulary [@Yuksel.etal.2020]. 
It is not clear how frequently flashcards are used to help computer language learners to learn programming syntax, but it seems likely that flashcards could be a useful tool in this endeavor [@Hermans.2021]. 

The goal of the `flashr` package is to provide a system to easily use and make flashcard decks. 
While digital flashcard creators exist, `flashr` already has [built-in flashcard decks](https://jeffreyrstevens.github.io/flashr_decks/decks.html) in place for learners to solidify their knowledge of R-specific syntax and concepts. 
In particular, it has decks for the chapters of a popular book used to learn R and the [tidyverse](https://www.tidyverse.org/), _R for Data Science_ by Wickham and Grolemund [-@Wickham.Grolemund.2017]. 
So learners can practice remembering the functions as they work their way through the book.
The built-in decks not only have terms and descriptions but also links to the documentation for each term.
This allows users to easily follow up to get more information about the terms.

In addition to built-in decks, users can create their own decks. 
First, they may create CSV files with columns of terms and descriptions, along with optional package names and links to documentation for each term. 
Users can then contribute their decks to the [`flashr_decks` repository](https://github.com/JeffreyRStevens/flashr_decks) as a community resource.
And, of course, they do not have to restrict their own decks to R or even programming languages. 
Alternatively, they may draw functions from a list of functions used by decks in the `flashr_decks` repository to build their own deck. 
This is easy to use because users do not have to construct their own descriptions.
However, they are limited to using functions already in the list.
Combining the creation of CSV files and drawing from the function list allows users to customize the decks to match their needs.

The `flashr` package provides an educational resource that can supplement other systems and techniques to help users learn R. 
It is also highly customizable, allowing users to create flashcards to learn whatever content they would like. 
Learners can harness the power of repetition to take the next step in their learning.

# Acknowledgments

This work was funded by US National Science Foundation grant NSF-1658837.

# References
