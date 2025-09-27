#' Check for potential taxonomic splits in a query
#'
#' @param x A character vector of species names to check. Usually from a database.
#' @param pubDate Integer. An year (e.g., 2019) used as a reference date from when to check potential taxonomic split
#' @param includeAll Logical; If \code{TRUE}, include all species described since `pubDate` regardless of if it is already included in the queried species list. Default is \code{FALSE}
#' @param verbose Logical; If \code{TRUE}, prints progress messages. Default is \code{TRUE}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is \code{cores = 1}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' @param exact Logical. Will search queried names for exact matches only (e.g., does not retrieve "Tantilla cf. melanocephala" when searching for "Tantilla melanocephala"). Default is \code{FALSE}.
#' 
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{query}: the original input names.
#'   \item \code{RDB}: the best-matching valid names according to The Reptile Database.
#'   \item \code{status}: a status label indicating the result of the match (\code{"check_split"}, \code{"up_to_date"}, \code{"not_found"}, or \code{"failed"}).
#' }
#' 
#' @examples
#' query <- c(
#' "Atractus dapsilis",
#' "Atractus trefauti",
#' "Atractus snethlageae",
#' "Tantilla melanocephala",
#' "Oxybelis aeneus",
#' "Oxybelis rutherfordi",
#' "Vieira-Alencar authoristicus",
#' "Oxybelis aeneus",
#' "Bothrops pauloensis")
#'
#'\donttest{
#'result <- reptSplitCheck(x=query,
#'                         pubDate = 2019,
#'                         cores = 2,
#'                         showProgress = FALSE)
#'
#'result <- reptSplitCheck(x=query,
#'                         pubDate = 2019,
#'                         cores = 2,
#'                         showProgress = FALSE,
#'                         includeAll = TRUE)
#' }
#' @export
#' 
reptSplitCheck <- function(x,
                           pubDate = NULL,
                           includeAll = FALSE,
                           verbose = TRUE,
                           cores = 1,
                           showProgress = TRUE,
                           exact = FALSE) {
  if(!is.character(x)){
    stop("Object x is not a character vector")
  }
  if(is.null(pubDate)){
    message("Argument pubDate not provided, setting to ", min(letsRept::allReptiles$year))
    pubDate <- as.integer(min(letsRept::allReptiles$year))
  }
  
  results <- safeParallel(x, function(spp) {
    splitCheck(spp, pubDate = pubDate, includeAll = includeAll, verbose = verbose, x = x, exact = exact)
  }, cores = cores,
  showProgress = showProgress)
  
  df_final <- do.call(rbind, results)
  
  if (is.null(df_final)) {
    warning("No results retrieved.")
    return(data.frame(query = character(), RDB = character(), status = character(), stringsAsFactors = FALSE))
  }
  
  return(df_final)
}
