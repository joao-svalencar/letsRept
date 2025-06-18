# CRAN submission: letsHerp 0.1.0

This is the first submission of the package `letsHerp` to CRAN.

## R CMD check results

I have run R CMD check on:  
- macOS 10.15.7, R 4.3.3  
- Ubuntu 22.04, R 4.3.2 (via GitHub Actions CI)

R CMD check results:  
- No ERRORs  
- No WARNINGs  
- 1 NOTE for "New submission," which is expected.

## Test coverage and documentation

- All exported functions are documented and tested using `testthat`.  
- I have included a `NEWS.md` file with the initial release notes.  
- Examples requiring internet access are wrapped in `\donttest{}` to avoid failures on CRAN’s offline test servers.  
- Tests that require internet access use `skip_on_cran()` and checks for `Sys.getenv("NOT_CRAN")` to ensure they do not run during CRAN testing but do run in local environments.

## Changes since last submission

- Updated package title to remove redundant "An R" at the beginning, now:  
  *"An Interface to the Reptile Database"*  
- Added a direct hyperlink to The Reptile Database in the DESCRIPTION `Description` field:  
  *Provides tools to retrieve and summarize taxonomic information and synonymy data for reptile species using data scraped from The Reptile Database website (<https://reptile-database.reptarium.cz/>).*  
- Improved message handling in functions by introducing a `verbose` argument to control progress output. All console messages now use `message()` or `warning()`, allowing users to suppress them if desired.
- Implemented warnings to highlight any species sampling error and how to extract failed species from original objects
- Implemented synonym search in `herpSearch` when binomial does not match a species valid name, returns a message and (if verbose = TRUE, default), prints the valid species information in the console.

## Notes for CRAN reviewers

- The package scrapes publicly accessible data from [The Reptile Database](https://reptile-database.reptarium.cz) using respectful user-agent headers.  
- Most package functionality requires internet access. To accommodate CRAN’s testing environment, examples and tests that depend on online resources are conditionally skipped as described above.  
- All console output can be controlled or suppressed by the user through the `verbose` argument or standard R message/warning handling.

Thank you for your time and consideration.
