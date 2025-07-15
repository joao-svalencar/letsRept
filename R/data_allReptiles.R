#' Example dataset: allReptiles
#'
#' This dataset contains the valid names and url addresses for all reptile species cataloged in The Reptile Database.
#'
#' @format A dataframe (download: May 23th, 2025) with 12440 rows and 8 variables:
#' \describe{
#'   \item{order}{A species current order}
#'   \item{suborder}{A species current suborder}
#'   \item{family}{A species current family}
#'   \item{genus}{A species current genus}
#'   \item{species}{A character vector with known current valid name for all reptile species cataloged in The Reptile Database website}
#'   \item{year}{A species description year}
#'   \item{author}{The authors that described the species under the current valid name}
#'   \item{url}{A character column with the respective url to access all reptile species cataloged in The Reptile Database website information}
#' }
#' @source The data was sampled from The Reptile Database website https://reptile-database.reptarium.cz using function letsRept::reptSpecies() with the url obtained from an 'Advanced search' set to exclude all reptile species described to the fictional planet Arrakis (-Arrakis).
#' 
"allReptiles"
