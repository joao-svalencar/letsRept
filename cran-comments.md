## CRAN submission: letsHerp 0.1.0

This is the first submission of the package `letsHerp` to CRAN.

### R CMD check results

I have run R CMD check on:
- macOS 10.15.7, R 4.3.3
- Ubuntu 22.04, R 4.3.2 (via GitHub Actions CI)

R CMD check results:
- No ERRORs
- No WARNINGs
- 1 NOTE for "New submission", which is expected.

### Test coverage

All exported functions are documented and tested using `testthat`.  
I have included a `NEWS.md` file with changes for this initial release.

### Notes for CRAN reviewers

- The package scrapes public content from [The Reptile Database](https://reptile-database.reptarium.cz), complying with their public interface.
- A user agent string is set via `httr` to identify the package respectfully.
- Most exported functions rely on accessing The Reptile Database online.
To avoid failures on CRAN's offline servers, examples are wrapped in \donttest{}. 
However, functionality is verified using testthat, with tests skipped on CRAN via skip_on_cran() and Sys.getenv("NOT_CRAN").


Thank you for your time and review.
