#' Check for possible splits in herp species names
#'
#' @param x A character vector of species names to check.
#' @param pubDate An integer year (e.g., 2019) used to detect new taxa.
#' @param verbose Logical; whether to print progress.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is one less than the total available cores.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#'
#' @return A data frame with columns: query, species, status.
#' @export
herpSplitCheck <- function(x, pubDate = NULL, verbose = TRUE, cores = max(1, parallel::detectCores() - 1), showProgress = TRUE) {
  results <- safeParallel(x, function(spp) {
    splitCheck(spp, pubDate = pubDate, verbose = verbose)
  }, cores = cores,
  showProgress = showProgress)
  
  df_final <- do.call(rbind, results)
  
  if (is.null(df_final)) {
    warning("No results retrieved.")
    return(data.frame(query = character(), species = character(), status = character(), stringsAsFactors = FALSE))
  }
  
  return(df_final)
}
