#' Example dataset: allSynonyms
#'
#' This dataset contains the valid names and respective listed synonyms for all reptile species cataloged in The Reptile Database.
#'
#' @format A dataframe with 55,130 rows and 2 variables:
#' \describe{
#'   \item{species}{A character vector with known current valid name for all reptile species cataloged in The Reptile Database website (download: March 27th, 2026)}
#'   \item{synonym}{A character column with the respective synonyms for all reptile species cataloged in The Reptile Database website information (download: March 27th, 2026)}
#' }
#' @source The data was sampled from The Reptile Database website https://reptile-database.reptarium.cz using function letsRept::reptSynonyms(letsRept::allReptiles) 
#' 
"allSynonyms"
