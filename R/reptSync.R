#' Synchronize Species Names Using The Reptile Database
#'
#' @description
#' Queries a list of species names through \code{reptSearch()} and returns a data frame with the currently valid names and taxonomic status for each input.
#'
#' @param x A character vector of taxon names to be matched (e.g., species lists, phylogenetic tip labels, or trait table entries).
#' @param solveAmbiguity Logical. If \code{TRUE}, attempts to resolve ambiguous names by retrieving all possible valid species to which the query may refer. Default is \code{TRUE}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is half of available cores (min = 1).
#' @param showProgress Logical. If \code{TRUE}, displays progress updates during processing. Default is \code{TRUE}.
#' @param getLink Logical. If \code{TRUE}, retrieves searched species URLs. Defaults if \code{FALSE}.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{query}: the original input names.
#'   \item \code{RDB}: the best-matching valid names according to The Reptile Database.
#'   \item \code{status}: a status label indicating the result of the match (\code{"up_to_date"}, \code{"updated"}, \code{"ambiguous"}, or \code{"not_found"}).
#'   \item \code{url}: Optional, if getLink = TRUE returns the URL of the species page retrieved for each match, or a list of possible matches if ambiguous.
#' }
#'
#' @note
#' \code{reptSync()} does not make authoritative taxonomic decisions. It matches input names against currently accepted names in The Reptile Database (RDB). 
#' A name marked as \code{"up_to_date"} may still refer to a taxon that has been split, and thus may not reflect the most recent population-level taxonomy.
#'
#' For ambiguous names, the \code{url} field may contain multiple links corresponding to all valid species to which the queried name is considered a synonym.
#' 
#' See package vignettes for more details.
#'
#' @references
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#'
#' @examples
#' query <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
#' 
#' \donttest{
#' reptSync(x = query, cores = 2)
#' }
#'
#' @export

reptSync <- function(x, 
                     solveAmbiguity = TRUE,
                     cores = max(1L, floor(parallel::detectCores() / 2)),
                     showProgress = TRUE,
                     getLink = FALSE) {
  
  # Worker function: performs search + classifies result
  worker <- function(species_name) {
    result <- letsRept::reptSearch(species_name)
    
    if (is.list(result)) {
      RDB <- result$species
      status <- if (species_name == RDB) "up_to_date" else "updated"
      url <- result$url
    } else if (is.character(result) && grepl("^https:", result)) {
      search <- rvest::read_html(result)
      title_node <- rvest::html_element(search, "h1")
      title_text <- rvest::html_text(title_node, trim = TRUE)
      
      if (grepl("^Search results", title_text)) {
        ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
        if (length(ul_element) == 0) return(NULL)
        
        li_nodes <- xml2::xml_children(ul_element[[1]])
        species <- sapply(li_nodes, function(node) {
          rvest::html_text(rvest::html_element(xml2::xml_child(node, 1), "em"), trim = TRUE)
        })
      }
      RDB <- paste(species, collapse = "; ")
      status <- "ambiguous"
      url <- result
    } else {
      RDB <- result
      status <- "not_found"
      url <- result
    }
    data.frame(query = species_name, RDB = RDB, status = status, url = url, stringsAsFactors = FALSE)
  }
  
  # Run in parallel using your safeParallel() function
  results <- safeParallel(x, FUN = worker, cores = cores, showProgress = showProgress)
  
  # Combine all individual data frames into one
  df <- do.call(rbind, results)
  
  if(solveAmbiguity){
    
    synSample <- df[df$status == "ambiguous", c("query", "url")]
    if(showProgress && nrow(synSample) >=1 ) message(sprintf("Sampling synonyms to approach ambiguity for %d species", nrow(synSample)))
    
    if (nrow(synSample) > 0) {
      ambiguity_results <- safeParallel(1:nrow(synSample), FUN = function(i) {
        # For each species, resolve ambiguity using reptSynonyms
        spp_syn <- reptSynonyms(reptSpecies(synSample$url[i], getLink = TRUE, showProgress = FALSE, cores = cores), cores = cores, showProgress = FALSE)
        synonyms <- spp_syn$species[synSample$query[i] == spp_syn$synonyms]
        
        if (length(synonyms) == 1) {
          RDB_new <- synonyms
          status_new <- "updated"
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
    df$status[df$RDB %in% names(which(table(df$RDB)[!names(table(df$RDB)) %in% c("ambiguous", "not_found")] >=2))] <- "synonymization"
    if(getLink){
      return(df)  
    }else{
      df <- df[,-4]
      return(df)  
    }
  }else{
    df$status[df$RDB %in% names(which(table(df$RDB)[!names(table(df$RDB)) %in% c("ambiguous", "not_found")] >=2))] <- "synonymization"
    if(getLink){
      return(df)  
    }else{
      df <- df[,-4]
      return(df)
    }
  }
}
