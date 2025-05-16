#' Example dataset: allReptiles
#'
#' This dataset contains the valid names and url addresses for all reptile species cataloged in The Reptile Database.
#'
#' @format A dataframe with 12384 rows and 2 variables:
#' \describe{
#'   \item{species}{A character vector with known current valid name for all reptile species cataloged in The Reptile Database website (download: May 15th, 2025)}
#'   \item{url}{A character column with the respective url to access all reptile species cataloged in The Reptile Database website information (download: May 15th, 2025)}
#' }
#' @source The data was sampled from The Reptile Database website https://reptile-database.reptarium.cz using function HerpNom::getSpecies() with the url obtained from an 'Advanced search' set to exclude all reptile species described to the fictional planet Arrakis (-Arrakis).
#' 
"allReptiles"
