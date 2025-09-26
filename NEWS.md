# letsRept NEWS
The package was renamed to `letsRept`

## Version 1.x.x — 2025-XX-XX
- Official manuscript release
- safeParallel modified to safely stop all clusters upon user interruption
- implemented safeRequest to safely address server workload
- reptSynonyms nows works for basic list of valid names, does not require species links. Also only runs with parallel sampling.
- reptSync status "synonymization" changed to "merge"
- improved functions documentation (corrected errors and missing information)
- improved reptAdvancedSearch output in case of detecting single species (when searching for synonyms) = reptSearch
- updated internal datasets (RDB September 2025 version)

## Version 1.0.1 — 2025-08-23
- Fixed CRAN test issues by skipping network-dependent tests.
- Made parallel calls safe on macOS (cores = 1 for tests).
- Minor bugfixes and test updates.

## Version 1.0.0 — 2025-08-05

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
