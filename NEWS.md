# letsRept NEWS

## Version 1.1.0 — 2025-10-20
- General:
  - Official manuscript release https://journals.ku.edu/jbi/article/view/24329
  - Package website release https://joao-svalencar.github.io/letsRept/
  - improved functions documentation (corrected errors and missing information)
  - updated internal datasets (RDB September 2025 version)
  - updated vignettes

- internal functions: 
  - safeParallel modified to safely stop all clusters upon user interruption
  - implemented safeRequest to safely address server workload
  - clean_species_names, fixed hyphened names like "Simotes tri-notatus"
  
- reptAdvancedSearch:
  - improved output in case of detecting single species (when searching for synonyms) = reptSearch
  
- reptCompare:   
  - implemented argument `compareDataset` to detect missing names in comparison with another dataset

-reptSpecies:
  - Only runs with parallel sampling, default is cores = 1.
  
- reptSplitCheck:
  - implemented argument `exact` for optional search only for exact matches
  
- reptSync:
  - status "synonymization" changed to "merge"
  - try fuzzy match before giving status "not_found"
  - added statuses "updated_typo" and "fuzzy_ambiguous" after fuzzy match
  
- reptSynonyms:
  - Now works with vectors of valid names, does not require species links.
  - Only runs with parallel sampling, default is cores = 1.

## Version 1.0.1 — 2025-08-23
- Fixed CRAN test issues by skipping network-dependent tests.
- Made parallel calls safe on macOS (cores = 1 for tests).
- Minor bugfixes and test updates.

## Version 1.0.0 — 2025-08-05

The package was renamed to `letsRept`

### Major changes

- `reptSync()` was fully redesigned for improved performance and flexibility. This change breaks compatibility with previous versions.
- Added new function `reptSplitCheck()`. This function checks for potential taxonomic splits departing from a user defined date.
- Added new function `reptTidySyn()`. This function prints to the console the outcomes of `reptSync` and `reptSplitCheck` with an optional "filter" argument.
- Added new function `reptCompare()`. This function compares a user species list with the internal dataset or with a subset provided by users
- Added new function `reptRefs()`. This function retrieves the references and links to access them from a species account.
- Added new function `reptStats()`. This function summarizes higher taxa information.
- Added helper `splitCheck()` to support `reptSplitCheck()`.

### Other changes
- I updated and expanded the vignettes to include the major changes

---
