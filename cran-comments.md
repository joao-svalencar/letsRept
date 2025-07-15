# CRAN submission: letsRept 0.0.1

This package is a renamed and refactored version of the previously submitted package `letsHerp`.
It is submitted as a new package due to the name change, and supersedes the previous version.

## R CMD check results

I have run R CMD check on:  
- macOS 10.15.7, R 4.3.3  
- Ubuntu 22.04, R 4.3.2 (via GitHub Actions CI)

R CMD check results:  
- No ERRORs  
- No WARNINGs  
- No Notes

## Test coverage and documentation

- All exported functions are documented and tested using `testthat`.  
- A `NEWS.md` file is included with a summary of major changes.

## Summary of changes in comparison to letsHerp (version 0.0.1)

- `herpSync()`, now `reptSync()` was fully redesigned for improved performance and flexibility.
- Added `reptSplitCheck()` to identify potential taxonomic splits after a user-defined year.
- Added `reptTidySyn()` to summarize outputs from `reptSync()` and `reptSplitCheck()`, with optional filtering.
- Added `reptCompare()` to facilitate the comparison of a user species list with the internal dataset or with a subset provided by users
- Added `reptRefs()` to retrieve the references and links to access them from a species account.
- Added `reptStats()` to summarize higher taxa information.
- Added internal helper `splitCheck()` to support `reptSplitCheck`.
- Vignettes updated and expanded to reflect the changes.

## Notes for CRAN reviewers

- The package scrapes publicly accessible content from [The Reptile Database](https://reptile-database.reptarium.cz) using a respectful user-agent via `httr`.
- Most functions require internet access. To accommodate CRAN’s offline testing environment:
  - Examples are wrapped in `\donttest{}`.
  - Tests using online resources are skipped on CRAN using `skip_on_cran()` and `Sys.getenv("NOT_CRAN")`.
- Console output and progress feedback are now fully suppressible using `verbose` or `showProgress` arguments, where applicable.
- The only exception is `reptTidySyn()`, which is an interactive console summary function (similar to `print()` or `summary()`), and is intended to produce console output by design.

Thank you for your time and consideration.
