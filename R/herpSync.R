#' Synchronize species names using The Reptile Database
#'
#' @description
#' Queries a list of species parsed to \code{herpSearch} and returns a data frame with current valid names for queried species
#'
#' @usage
#' herpSync(x, solveAmbiguity = TRUE, cores = max(1, parallel::detectCores() - 1), verbose = TRUE, showProgress = TRUE, getLink = FALSE)
#'
#' @param x A character vector of taxon names to be processed (e.g., species list, phylogenetic tip labels, or trait table entries).
#' @param solveAmbiguity Logical. If \code{TRUE}, samples the synonyms of species with ambiguous nomenclature
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is one less than the number of available cores.
#' @param verbose Logical. If \code{TRUE}, prints species information in the console. Default is \code{TRUE}.
#' @param showProgress Logical. If \code{TRUE}, prints sampling progress in the console. Default is \code{FALSE}.
#' @param getLink Logical. If \code{TRUE}, includes the RDB URL for each valid species names detected Default is \code{FALSE}.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{query}: original input names from the query.
#'   \item \code{RDB}: best-matching valid names according to the RDB.
#'   \item \code{status}: description of the outcome (e.g., \code{up_to_date}, \code{"updated"}, \code{"ambiguous"},  \code{"not_found"}).
#'   \item \code{url}: the url searched for each querried species. The urls of species with ambiguous nomenclature returns a list of species of which the querried species is considered a synonym
#' }
#' 
#' @note
#' \code{herpSync} does not make taxonomic claims. 
#' The function compares queried species with valid species names and returns "up_to_date" for any binomial that is considered valid,
#' regardless of if it has been split and the name applies only to a specific population.
#' 
#' @references
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#'
#' @examples
#' query <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
#' \donttest{
#' herpSync(x=query)
#' }
#' 
#'
#' @export
#'
herpSync <- function(x, solveAmbiguity = TRUE, cores = max(1, parallel::detectCores() - 1), verbose = TRUE, showProgress = TRUE, getLink = FALSE) {
  
  # Worker function: performs search + classifies result
  worker <- function(species_name) {
    result <- letsHerp::herpSearch(species_name)
    
    if (is.list(result)) {
      RDB <- result$species
      status <- if (species_name == RDB) "up_to_date" else "updated"
      url <- result$url
    } else if (is.character(result) && grepl("^https:", result)) {
      RDB <- "ambiguous"
      status <- "ambiguous"
      url <- result
    } else {
      RDB <- result
      status <- "unknown"
      url <- result
    }
    data.frame(query = species_name, RDB = RDB, status = status, url = url, stringsAsFactors = FALSE)
  }
  
  # Run in parallel using your safeParallel() function
  results <- safeParallel(x, FUN = worker, cores = cores, showProgress = showProgress)
  
  # Combine all individual data frames into one
  df <- do.call(rbind, results)
  
  if(solveAmbiguity){
    
    synSample <- df[df$RDB == "ambiguous", c("query", "url")]
    if(showProgress) message(sprintf("Sampling synonyms to approach ambiguity for %d species", nrow(synSample)))
    
    if (nrow(synSample) > 0) {
      ambiguity_results <- safeParallel(1:nrow(synSample), FUN = function(i) {
        # For each species, resolve ambiguity using herpSynonyms
        spp_syn <- herpSynonyms(herpSpecies(synSample$url[i], getLink = TRUE, showProgress = FALSE), showProgress = FALSE)
        synonyms <- spp_syn$species[synSample$query[i] == spp_syn$synonyms]
        
        if (length(synonyms) == 1) {
          RDB_new <- synonyms
          status_new <- "updated"
          #url_new <- 
        } else if (length(synonyms) > 1) {
          RDB_new <- paste(synonyms, collapse = "; ")
          status_new <- "ambiguous"
        } else {
          RDB_new <- "not_found"
          status_new <- "not_found"
        }
        
        list(query = synSample$query[i], RDB_new = RDB_new, status_new = status_new)
      }, cores = cores, showProgress = showProgress)
      
      # Combine ambiguity results into a data frame
      ambiguity_df <- do.call(rbind, lapply(ambiguity_results, function(res) {
        data.frame(query = res$query, RDB = res$RDB_new, status = res$status_new, stringsAsFactors = FALSE)
      }))
      
      # Update the main dataframe with the resolved results
      for (i in 1:nrow(ambiguity_df)) {
        df$RDB[df$query == ambiguity_df$query[i]] <- ambiguity_df$RDB[i]
        df$status[df$query == ambiguity_df$query[i]] <- ambiguity_df$status[i]
      }
    }
    return(df)  
  }else{
    return(df) 
  }
}
