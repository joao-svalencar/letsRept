#' Check for potential taxonomic splits in a query
#'
#' @param x A character vector of species names to check. Usually from a database.
#' @param pubDate Integer. An year (e.g., 2019) used as a reference date from when to check potential taxonomic split
#' @param verbose Logical; If \code{TRUE}, prints progress messages. Default is \code{TRUE}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is half of available cores.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{query}: the original input names.
#'   \item \code{RDB}: the best-matching valid names according to The Reptile Database.
#'   \item \code{status}: a status label indicating the result of the match (\code{"up_to_date"}, \code{"updated"}, \code{"ambiguous"}, or \code{"not_found"}).
#' }
#' 
#' @examples
#' query <- c("Vieira-Alencar authoristicus",
#' "Tantilla melanocephala",
#' "Oxybelis aeneus",
#' "Apostolepis dimidiata",
#' "Bothrops pauloensis")
#'
#'\donttest{
#'result <- herpSplitCheck(x=query, pubDate = 2019, cores = 2, showProgress = FALSE)
#' }
#' @export
#' 
herpSplitCheck <- function(x,
                           pubDate = NULL,
                           verbose = TRUE,
                           cores = parallel::detectCores()/2,
                           showProgress = TRUE) {
  
  results <- safeParallel(x, function(spp) {
    splitCheck(spp, pubDate = pubDate, verbose = verbose)
  }, cores = cores,
  showProgress = showProgress)
  
  df_final <- do.call(rbind, results)
  
  if (is.null(df_final)) {
    warning("No results retrieved.")
    return(data.frame(query = character(), RDB = character(), status = character(), stringsAsFactors = FALSE))
  }
  
  return(df_final)
}
