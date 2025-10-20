# CRAN submission: letsRept 1.1.0

This new version includes some important news. The official manuscript has been released (https://journals.ku.edu/jbi/article/view/24329);
Some functions were upgraded

## R CMD check results

I have run R CMD check on:
- macOS 10.15.7, R 4.3.3  
- Ubuntu 22.04, R 4.3.2 (via GitHub Actions CI)

R CMD check results:  
- No ERRORs  
- No WARNINGs  
- No NOTEs

## Test coverage and documentation

- All exported functions are documented and tested using `testthat`.  
- A `NEWS.md` file is included with a summary of major changes.

## Summary of changes in comparison to letsHerp (version 1.0.0)

- Improved functions documentation
- Minor internal functions upgrade
- Parallel functions safely running in all OS
- Server requests modified reducing server overload
- `reptSync` statuses renamed and fuzzy match implemented to solve minor "not_found" status
- `reptCompare` and `reptSplitCheck` has new arguments to improve user experience and control
- Vignettes updated and expanded to reflect the changes.
- Package website released (https://joao-svalencar.github.io/letsRept/)

## Notes for CRAN reviewers

- The package scrapes publicly accessible content from [The Reptile Database](https://reptile-database.reptarium.cz) using a respectful user-agent via `httr`.
- Most functions require internet access. To accommodate CRAN’s offline testing environment:
  - Examples are wrapped in `\donttest{}`.
  - Tests using online resources are skipped on CRAN using `skip_on_cran()` and `Sys.getenv("NOT_CRAN")`.
- Console output and progress feedback are now fully suppressible using `verbose` or `showProgress` arguments, where applicable.
- The only exception is `reptTidySyn()`, which is an interactive console summary function (similar to `print()` or `summary()`), and is intended to produce console output by design.
- Considering that the package relies on The Reptile Database, I changed the CITATION file to include the reference to the website. I plan to edit the CITATION file again as soon as the manuscript related to the package gets published.

Thank you for your time and consideration.
