#' Compare species nomenclature between datasets or with Reptile Database Data
#'
#' This function compares a list of species (`x`) with another list (`y`), typically from the Reptile Database (RDB).
#' If `y` is not provided, it defaults to using the internal object `allReptiles`, extracted from RDB (version: May, 2025).
#' The function returns species from `x`that are either unmatched ("review") or matched with `y` or with `allReptiles`, if `y` is NULL.
#' If `y` is provided and any species from `x` is a valid name according to RDB but is absent from `y`, it receives the status `absent`
#'
#' @param x A character vector or a data frame containing a column named `species` with species names to be compared.
#' @param y Optional. A character vector or a data frame containing a column named `species` to be compared. Defaults to the internal object `allReptiles`.
#' @param filter Optional. A character string or a vector of characters.
#' If `"review"`, returns only unmatched species.
#' If `"matched"`, returns only matched species.
#' If `"absent"`, returns only species from `x` that are absent from `y`.
#' If `NULL` (default), returns a data frame with all species and respective statuses.
#' Multiple filters can be concatenated and the resulting data frame will return all species with both status.
#' @param compareDataset Logical. If TRUE, assumes all input names are up-to-date and
#' compares the input list with the reference database to identify any missing species.
#'
#' @return A character vector (if `filter` is `"review"`, `"matched"` or `"absent`), or a data frame with columns:
#' \describe{
#'   \item{species}{Species names from `x`}
#'   \item{status}{Comparison result: `"review"`, `"matched"` or `"absent"`}
#' }
#' 
#' If compareDataset = \code{TRUE}, then the function returns a vector of species from `y` that is absent from `x`.
#'
#' @examples
#' my_species <- data.frame(species = c("Boa constrictor", "Pantherophis guttatus", "Fake species"))
#' reptCompare(my_species)
#' reptCompare(my_species, filter = "review")
#' reptCompare(my_species, filter = "matched")
#'
#' @export

reptCompare <- function(x = NULL, y = NULL, filter = NULL, compareDataset = FALSE){

  if(is.null(x)){
    stop("No species list provided")
  }
  
  if(is.data.frame(x)){
    if(!"species" %in% names(x)){
      stop("No column 'species' detected")
    }else{
      x <- x$species
    }
  }
    #cleaning all random white spaces:
    x <- gsub("\\p{Zs}+", " ", x, perl = TRUE)
    x <- trimws(x)
    x <- gsub(" +", " ", x)

        if(compareDataset){
          if(is.null(y)){
            stop("No dataset provided to compare with.")
          }else{
              if(is.data.frame(y)){
                if(!"species" %in% names(y)){
                  stop("No column 'species' detected")
                }else{
                y <- y$species
              }
            }else{
              absent_from_query <- y[which(!y %in% x)]
              return(absent_from_query)
            }
          }
        }
    
    if(is.null(y)){
      message("No RDB list provided, comparing with internal data 'allReptiles'")
      y <- letsRept::allReptiles
    }

  if(is.data.frame(y)){
    if(!"species" %in% names(y)){
      stop("No column 'species' detected")
    }else{
      y <- y$species
    }
  }
  review <- x[which(!x %in% y)]
  matched <- x[which(x %in% y)]

  absent <- review[review %in% letsRept::allReptiles$species]
  review <- review[!review %in% letsRept::allReptiles$species] #removing `absent` species from review
    
  if(length(review) == 0 && length(absent) == 0){
    message("\nAll species nomenclature are up to date!")
    matched <- data.frame(species = matched, status = "matched")
    return(matched)
  }

  if(!is.null(filter)){
    valid_filters <- c("review", "matched", "absent")
    if(!all(filter %in% valid_filters)){
      stop("No valid filter argument provided")
    }

    # for filtering one status:
    if (length(filter) == 1) {
          if (filter == "review"){
            if(length(review) > 0){
            return(review)
              }else {
                stop("No species with 'review status'")
              }
            }

          if (filter == "matched"){
            if(length(matched) > 0){
            return(matched)
              }else {
                stop("No species with 'matched status'")
            }
          }

          if (filter == "absent"){
            if(length(absent) > 0){
            return(absent)
              }else {
                stop("No species with 'absent status'")
            }
          }
    } else{

    #for filtering multiple status:
    df <- data.frame()
    if ("review" %in% filter) df <- rbind(df, data.frame(species = review, status = "review"))
    if ("matched" %in% filter) df <- rbind(df, data.frame(species = matched, status = "matched"))
    if ("absent" %in% filter) df <- rbind(df, data.frame(species = absent, status = "absent"))

    df <- df[order(df$species), ]
    row.names(df) <- seq_along(df$species)
    return(df)
    }
  }else{

    #for no status filtering:
    review <- data.frame(species = review, status = "review")
    matched <- data.frame(species = matched, status = "matched")

    if(length(absent) != 0){
      absent <- data.frame(species = absent, status = "absent")
      df <- rbind(review, matched, absent)
    }else{
      df <- rbind(review, matched)
    }

    df <- df[order(df$species),]
    row.names(df) <- seq_along(df$species)
    return(df)
  }
}