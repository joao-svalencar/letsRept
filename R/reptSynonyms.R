#' Retrieve Synonyms for Reptile Species from RDB
#' 
#' @description
#' Retrieves a data frame containing the current valid names of reptile species along with all their recognized synonyms, as listed in The Reptile Database (RDB). 
#' Optionally, the references citing each synonym can also be included.
#' 
#' @param x A character string with a species binomial or a data frame with columns \code{species} and \code{url}, typically the output of \code{\link{reptSpecies}} with \code{getLink = TRUE}.
#' @param getRef Logical. If \code{TRUE}, includes the reference(s) in which each synonym was mentioned. Default is \code{FALSE}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is \code{cores = 1}.
#'
#' @return
#' A data frame with columns:
#' \itemize{
#'   \item \code{species}: The valid species name according to RDB.
#'   \item \code{synonym}: Recognized synonyms and chresonyms for the species. Chresonyms are usually separated from authors with an emdash.
#'   \item \code{reference} (optional): If \code{getRef = TRUE}, the citation where the synonym was reported.
#' }
#'
#' @references 
#' Uetz, P., Freed, P., Aguilar, R., Reyes, F., Kudera, J., & Hošek, J. (eds.) (2025). The Reptile Database. Retrieved from \url{http://www.reptile-database.org}  
#'
#' @examples
#' # Filter species belonging to genus Boa
#' boa <- letsRept::allReptiles[grep("^Boa\\s", letsRept::allReptiles$species), ]
#' 
#' \donttest{
#' # Retrieve synonyms (without references)
#' boa_syn <- reptSynonyms(boa, getRef = FALSE, cores = 2)
#' Bconstrictor_syn <- reptSynonyms(x = "Boa constrictor")
#' }
#' 
#' @seealso \code{\link{reptSpecies}}, \code{\link{reptAdvancedSearch}}
#'
#' @export


reptSynonyms <- function(x, 
                         getRef=FALSE,
                         showProgress = TRUE,
                         cores = 1)
{
    if (is.character(x)) {
      # case: just a vector of binomials
        synonyms <- safeParallel(
          data = seq_along(x),
          FUN = function(i) getSynonymsParallel_vector(x[i], getRef = getRef),
          cores = cores,
          showProgress = showProgress
        )
        df <- do.call(rbind, synonyms)
      
      if (!getRef) {
        df$combined <- paste(df$species, df$synonyms, sep = "_")
        df <- data.frame(unique(df$combined))
        df <- tidyr::separate(df, col = "unique.df.combined.", into = c("species", "synonyms"), sep = "_", convert = TRUE)
        # warning for failed species
        n_failed <- sum(df$synonyms == "failed")
        
        if (n_failed > 0) {
          failed_spp <- unique(df$species[df$synonyms == "failed"])
          msg <- sprintf(
            "%d species failed. Retrieve failed species and URLs using:\nx[x$species %%in%% c(%s), c(\"species\", \"url\")]",
            n_failed,
            paste(shQuote(failed_spp), collapse = ", ")
          )
          warning(msg, call. = FALSE)
        }
        if(showProgress == TRUE){
          cat("\nSynonyms sampling complete!\n")
        }
      }
      return(df)
    }
    
    if(is.data.frame(x)){
      if (!"url" %in% names(x) || all(is.na(x$url))) {
        stop("No valid species URL found in data frame. Try using species names vector or provide x with column 'url'")
      }

    synonyms <- safeParallel(
      data = seq_along(x$species),
      FUN = function(i) getSynonymsParallel(
        i,
        x = x,
        getRef = getRef
        ),
      cores = cores,
      showProgress = showProgress
    )
    df <- do.call(rbind, synonyms)
    
    if (!getRef) {
      df$combined <- paste(df$species, df$synonyms, sep = "_")
      df <- data.frame(unique(df$combined))
      df <- tidyr::separate(df, col = "unique.df.combined.", into = c("species", "synonyms"), sep = "_", convert = TRUE)
      
      # warning for failed species
      n_failed <- sum(df$synonyms == "failed")
      
      if (n_failed > 0) {
        failed_spp <- unique(df$species[df$synonyms == "failed"])
        msg <- sprintf(
          "%d species failed. Retrieve failed species and URLs using:\nx[x$species %%in%% c(%s), c(\"species\", \"url\")]",
          n_failed,
          paste(shQuote(failed_spp), collapse = ", ")
        )
        warning(msg, call. = FALSE)
      }
      if(showProgress == TRUE){
      cat("\nSynonyms sampling complete!\n")
      }
    }
    return(df)
    }
}
