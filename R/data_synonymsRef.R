#' Example dataset: allSynonymsRef
#'
#' This dataset contains the valid names and respective listed synonyms for all reptile species cataloged in The Reptile Database.
#'
#' @format A dataframe with 110,413 rows and 3 variables:
#' \describe{
#'   \item{species}{A character vector with known current valid name for all reptile species cataloged in The Reptile Database website (download: May 23rd, 2025)}
#'   \item{synonym}{A character column with the respective synonyms for all reptile species cataloged in The Reptile Database website information (download: May 23rd, 2025)}
#'   \item{ref}{A charater column with the synonyms and respective references that used it}
#' }
#' @source The data was sampled from The Reptile Database website https://reptile-database.reptarium.cz using function letsRept::reptSynonyms(letsRept::allReptiles) 
#' 
"allSynonymsRef"
