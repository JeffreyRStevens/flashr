# flashr 0.0.4

* Users can create their own decks by drawing functions from the [`flashr_decks` repo function list](https://jeffreyrstevens.github.io/flashr_decks/functions.html)
* Update links to the [`flashr_decks` website](https://jeffreyrstevens.github.io/flashr_decks/)
* Fix bug that caused fatal error when `flashcard()` was applied to decks with no package for items

# flashr 0.0.3

* When listing decks, read in a CSV of available decks rather than reading in all of the actual decks
* Allow users to change font color of text and links
* Post screen capture video example to README
* Fix bug of `{NA}` output when `package = TRUE` but no package is included for term

# flashr 0.0.2

* Include column of URLs to CSV files that create links to documentation for terms
* Allow users to customize font size of presentations
* Allow output directly to default browser rather than RStudio viewer
* Add precommit hooks to reduce commit errors
* Add codemeta.json

# flashr 0.0.1

* Create a `NEWS.md` file to track changes to the package.
