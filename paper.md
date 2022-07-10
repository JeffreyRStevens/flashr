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
date: 2022-07-07
bibliography: paper.bib
---

# Summary

Flashcards can be an effective way to learn new material, including vocabulary, spoken languages, and computer languages. The `flashr` package creates decks of flashcards from existing sets of terms and descriptions built around learning R. This package draws from a repository of decks based on existing books and resources. New decks are easy to build, and users can create new decks either for their own use or to contribute to the repository. Moreover, the decks do not have to be about R or even computing languages. Users can develop their own decks for whatever material they would like to learn.


# Statement of need

Learning a new language--either spoken languages or computer programming languages--can require a lot of time to map new vocabulary words to their definitions. Repetition is critical to learning new vocabulary [@Laufer.etal.2005; @Larsen-Freeman.2012], and the repeated use of flashcards can provide an effective way to learn technical vocabulary [@Yuksel.etal.2020]. 

* Importance of repetition
* Usefulness of flashcards

* Though digital flashcards systems exist, this package has R-specific terms and descriptions

* But it also allows customized decks

Researchers who conduct online surveys may use [Qualtrics](https://qualtrics.com) or other online systems to collect data from participants. Those participants may be recruited directly via listservs or through third party vendors that connect researchers and participants, such as [Amazon Mechanical Turk](https://www.mturk.com/) and [Prolific](https://prolific.co/). Ensuring good data quality from these participants can be tricky [@Aruguete.etal.2019;@Chmielewski.Kucker.2020;@Gupta.etal.2021;@Eyal.etal.2021]. For instance, while Mechanical Turk in theory screens workers based on location (e.g., if you want to restrict your participant pool to workers in the United States), this is not necessarily represented in the data when participant IP addresses are recorded. Also, automated bots are constantly trying to complete online surveys with worthless data. Therefore, researchers may want to screen their data for certain exclusion criteria.

Finding the tools to screen for IP address location can be difficult, and the `excluder` package simplifies working with exclusion criteria based on data that Qualtrics reports, including geolocation, IP address, duplicate records from the same location, participant screen resolution, participant progress through the survey, and survey completion duration. `excluder` is an R [@RCoreTeam.2021] package based on the `tidyverse` [@Wickham.etal.2019] framework that use three primary functions to (1) mark existing files with new columns that flag data rows meeting exclusion criteria, (2) view the subset of data rows that meet exclusion criteria, and (3) exclude data rows that meet exclusion criteria from the data. In addition, `excluder` helps prepare Qualtrics data for analysis and can deidentify the data by removing columns with potentially identifiable information. Though the functionality focuses on data collected by Qualtrics and imported by the `qualtRics` [@Ginn.Silge.2021] package, it is flexible enough for researchers using any source of online survey data.

# Acknowledgments

This work was funded by US National Science Foundation grant NSF-1658837.

# References
