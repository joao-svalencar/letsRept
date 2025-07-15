#' Compare a Species nomenclature with Reptile Database Data
#'
#' This function compares a list of species (`x`) with another list (`y`), typically from the Reptile Database (RDB).
#' If `y` is not provided, it defaults to using the internal object `allReptiles`.
#' The function returns species that are either unmatched ("review") or matched with the RDB list, or both.
#'
#' @param x A character vector or a data frame containing a column named `species` with species names to be compared.
#' @param y Optional. A character vector or a data frame containing a column named `species` to compare against. Defaults to the internal object `allReptiles`.
#' @param filter Optional. A character string. If `"review"`, returns only unmatched species.
#' If `"matched"`, returns only matched species. If `NULL` (default), returns a data frame of both.
#'
#' @return A character vector (if `filter` is `"review"` or `"matched"`), or a data frame with columns:
#' \describe{
#'   \item{species}{Species names from `x`}
#'   \item{status}{Comparison result: `"review"` or `"matched"`}
#' }
#'
#' @examples
#' my_species <- data.frame(species = c("Boa constrictor", "Pantherophis guttatus", "Fake species"))
#' reptCompare(my_species)
#' reptCompare(my_species, filter = "review")
#' reptCompare(my_species, filter = "matched")
#'
#' @export

reptCompare <- function(x = NULL, y = NULL, filter = NULL){
  
  if(is.null(x)){
    stop("No species list provided")
  }
  if(is.null(y)){
    warning("No RDB list provided, comparing with internal data 'allReptiles'")
    y <- letsRept::allReptiles
  }
  
  if(is.data.frame(x)){
    x <- x$species
  }
  
  if(is.data.frame(y)){
    y <- y$species
  }
  
  review <- x[which(!x %in% y)]
  matched <- x[which(x %in% y)]
  
    if(!is.null(filter)){
        if(filter=="review"){
          return(review)
          }else
        if(filter=="matched"){
            return(matched)
          }else{
        stop("No valid filter argument provided")
        }
    }else{
      review <- data.frame(species = review, status = "review")
      matched <- data.frame(species = matched, status = "matched")
      
      df <- rbind(review, matched)
      df <- df[order(df$species),]
      return(df)
    }
}

