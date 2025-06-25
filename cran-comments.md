# CRAN submission: letsHerp 1.0.0

This is the second submission of the package `letsHerp` to CRAN.

## R CMD check results

I have run R CMD check on:  
- macOS 10.15.7, R 4.3.3  
- Ubuntu 22.04, R 4.3.2 (via GitHub Actions CI)

R CMD check results:  
- No ERRORs  
- No WARNINGs  
- 1 NOTE about "Days since last update", which is expected due to the recent 0.1.0 submission. This is a significant update with breaking changes.

## Test coverage and documentation

- All exported functions are documented and tested using `testthat`.  
- A `NEWS.md` file is included with a summary of major changes.

## Summary of changes in version 1.0.0

- `herpSync()` was fully redesigned for improved performance and flexibility (**breaking change**).
- Added `herpSplitCheck()` to identify potential taxonomic splits after a user-defined year.
- Added `herpTidySyn()` to summarize outputs from `herpSync()` and `herpSplitCheck()`, with optional filtering.
- Added internal helper `splitCheck()` to support `herpSplitCheck`.
- Vignettes updated and expanded to reflect the changes.

## Notes for CRAN reviewers

- The package scrapes publicly accessible content from [The Reptile Database](https://reptile-database.reptarium.cz) using a respectful user-agent via `httr`.
- Most functions require internet access. To accommodate CRAN’s offline testing environment:
  - Examples are wrapped in `\donttest{}`.
  - Tests using online resources are skipped on CRAN using `skip_on_cran()` and `Sys.getenv("NOT_CRAN")`.
- Console output and progress feedback are now fully suppressible using `verbose` or `showProgress` arguments, where applicable.
- The only exception is `herpTidySyn()`, which is an interactive console summary function (similar to `print()` or `summary()`), and is intended to produce console output by design.
- This update is submitted shortly after the previous version to address important functional changes and new features that were not included in version 0.1.0.

Thank you for your time and consideration.
