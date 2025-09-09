#' Summarize Taxonomic Composition
#'
#' This function summarizes the taxonomic content of a species list, typically an object created with `reptSpecies` with higher taxa information.
#' If no object is provided it summarizes the internal dataset `allReptiles`.
#' 
#' The output can be either a compact table with taxonomic unit counts or a verbose list of names within each rank.
#'
#' Optional arguments allow the user to filter the dataset by specific taxonomic levels (e.g., order, suborder, family, genus) before summarizing.
#'
#' @param x A data frame containing reptile taxonomy data. Defaults to the internal dataset `letsRept::allReptiles`.
#' @param verbose Logical. If `TRUE`, returns a list of taxon names by rank. If `FALSE` (default), returns a summary table of counts.
#' @param order Optional. A character string specifying a taxonomic order to filter by (e.g., `"Squamata"`).
#' @param suborder Optional. A character string specifying a taxonomic suborder to filter by (e.g., `"Serpentes"`).
#' @param family Optional. A character string specifying a family to filter by (e.g., `"Elapidae"`).
#' @param genus Optional. A character string specifying a genus to filter by (e.g., `"Micrurus"`).
#'
#' @return Either a named list of taxonomic units (`verbose = TRUE`) or a data frame with taxonomic ranks and the number of units per rank (`verbose = FALSE`).
#'
#' @examples
#' # Basic usage with default dataset
#' reptStats()
#'
#' # Verbose summary listing elements in each rank
#' reptStats(verbose = TRUE)
#'
#' # Filter by family and return summary table
#' reptStats(family = "Elapidae")
#'
#' # Combine filters and return list
#' reptStats(suborder = "Serpentes", verbose = TRUE)
#'
#' @export
reptStats <- function(x = letsRept::allReptiles,
                      verbose = FALSE,
                      order = NULL,
                      suborder = NULL,
                      family = NULL,
                      genus = NULL) {
  
  # Filter RDB based on user input
  if(!is.null(order))    x <- x[x$order == order, ]
  if(!is.null(suborder)) x <- x[x$suborder == suborder, ]
  if(!is.null(family))   x <- x[x$family == family, ]
  if(!is.null(genus))    x <- x[x$genus == genus, ]
  
  x <- x[!is.na(x$species), ]
  
  filters <- c(order = !is.null(order),
               suborder = !is.null(suborder),
               family = !is.null(family),
               genus = !is.null(genus))
  
  if(any(filters)){
    highest_filter <- max(which(filters))
  } else {
    highest_filter <- 1  
  }
  
  taxonomic_levels <- c("order", "suborder", "family", "genus", "species")
  levels_to_summarize <- taxonomic_levels[highest_filter:length(taxonomic_levels)]
  
  out <- lapply(x[, levels_to_summarize, drop = FALSE], function(col) {
    res <- sort(table(col), decreasing = TRUE)
    names(dimnames(res)) <- NULL
    res
  })
  
  out$species <- names(out$species)
  
  if(verbose){
    return(out)
  } else {
    out <- utils::stack(lapply(out, length))
    names(out) <- c("total", "taxa")
    out <- out[, c("taxa", "total")]
    out$taxa <- levels_to_summarize
    
    if(any(filters)){
      out <- out[-1, ]
    }
    return(out)
  }
}
