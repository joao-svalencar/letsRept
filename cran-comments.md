# CRAN submission: letsRept 1.1.1

This new version includes an update on the package internal datasets reflecting current Reptile taxonomy

## R CMD check results

I have run R CMD check with the following results:

- No ERRORs  
- No WARNINGs  
- One note: "unable to verify current time"

## Test coverage and documentation

- All exported functions are documented and tested using `testthat`.  
- A `NEWS.md` file is included with a summary of major changes.

## Summary of changes in comparison to letsRept (version 1.1.0)

- Minor internal functions upgrade
- Internal datasets upgrade

## Notes for CRAN reviewers

- The package scrapes publicly accessible content from [The Reptile Database](https://reptile-database.reptarium.cz) using a respectful user-agent via `httr`.
- Most functions require internet access. To accommodate CRAN’s offline testing environment:
  - Examples are wrapped in `\donttest{}`.
  - Tests using online resources are skipped on CRAN using `skip_on_cran()` and `Sys.getenv("NOT_CRAN")`.
- Console output and progress feedback are now fully suppressible using `verbose` or `showProgress` arguments, where applicable.
- The only exception is `reptTidySyn()`, which is an interactive console summary function (similar to `print()` or `summary()`), and is intended to produce console output by design.
- I edited the CITATION file again to add the official manuscript citation

Thank you for your time and consideration.
