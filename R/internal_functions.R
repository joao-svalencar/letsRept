#' Matching higher taxa information
#' 
#' @description
#' A helper function to herpSpecies(). Identify species orders or suborders from taxa_vector
#' 
#' @param taxa_vector A character vector containing taxonomic information.
#' @param rank_list A character vector of taxonomic rank names to match.
#'
#' @returns A character string of matched ranks, comma-separated. Returns NA if none found.
#' @keywords internal
#' 
match_taxon <- function(taxa_vector, rank_list) {
  matches <- rank_list[sapply(rank_list, function(rank) stringr::str_detect(taxa_vector, rank))]
  if(length(matches)==0){
    return(NA)
  }
  match_positions <- sapply(matches, function(rank) stringr::str_locate(taxa_vector, rank)[1])
  sorted_matches <- matches[order(match_positions)]
  return(paste(sorted_matches, collapse = ", "))
}




#' Applying parallel sampling
#' 
#' @description
#' A helper function to herpSpecies(). Decides which parallel template to follow according to user OS
#'
#' @param data species_list to be sampled
#' @param FUN function to be sampled in parallel
#' @param cores number of cores passed to main function 
#'
#' @returns the result of FUN performed in parallel running within user defined number of cores
#' @keywords internal
#' 
safeParallel <- function(data, FUN, cores = cores) {
  if (.Platform$OS.type == "unix") {
    parallel::mclapply(data, FUN, mc.cores = cores)
  } else {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    parallel::parLapply(cl, data, FUN)
  }
}


#' Scrape taxonomic info from the Reptile Database for one species
#'
#' Internal helper to extract order, suborder, family, and author info from a species page.
#'
#' @param x Species name to match.
#' @param species_list Vector of species names.
#' @param genus_list Vector of genus names.
#' @param url_list Vector of corresponding species URLs.
#' @param orders Vector with orders to be matched with taxa_vector
#' @param suborders Vector with suborders to be matched with taxa_vector
#' @param fullHigher Logical, whether to return full taxonomic string.
#' @param getLink Logical, whether to include the species page URL.
#'
#' @return A one-row data frame with taxonomic information. Returns NULL on error.
#' @keywords internal
#' 
#' 
higherSample <- function(x, species_list, genus_list, url_list, orders = orders, suborders = suborders, fullHigher = FALSE, getLink = FALSE) {
  
  j <- which(species_list == x)
  tryCatch({
    sp_page <- rvest::read_html(url_list[j])
    title <- rvest::html_element(sp_page, "h1")
    
    sppAuthor <- rvest::html_text(title, trim = TRUE)
    sppAuthor <- gsub("^([A-Z][a-z]+\\s+[a-z\\-]+)\\s*", "", sppAuthor)
    sppAuthor <- gsub("\\s{2,}", " ", sppAuthor)
    
    sppAuthor <- trimws(gsub("\\s+", " ", sppAuthor))
    
    sppYear <- stringr::str_extract(sppAuthor, "\\d{4}")
    
    element <- rvest::html_element(sp_page, "table")
    
    taxa <- xml2::xml_child(element, 1)
    td_taxa <- rvest::html_element(taxa, "td:nth-child(2)")
    children <- xml2::xml_contents(td_taxa)
    
    taxa_vector <- rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE)
    taxa_vector <- paste(taxa_vector, collapse = ", ")
    
    family <- stringr::str_extract(taxa_vector, "\\b[A-Z][a-z]+idae\\b")
    order <- match_taxon(taxa_vector, orders)
    suborder <- match_taxon(taxa_vector, suborders)
    
    return(list(
              order = order,
              suborder = suborder,
              family = family,
              genus = genus_list[j],
              species = species_list[j],
              author = sppAuthor,
              year = sppYear,
              taxa_vector = if (fullHigher) taxa_vector else NULL,
              url = if (getLink) url_list[j] else NULL,
              stringsAsFactors = FALSE
              ))
    
  }, error = function(e) {
    return(list(error = TRUE, species = species_list[j], message = e$message))
  })
}