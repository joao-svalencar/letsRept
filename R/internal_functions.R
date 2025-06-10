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
#' @noRd
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
#' @noRd
#' 

safeParallel <- function(data, FUN, cores = 1, showProgress = TRUE) {
  if (.Platform$OS.type == "unix") {
    if (showProgress && requireNamespace("pbmcapply", quietly = TRUE)) {
      pbmcapply::pbmclapply(data, FUN, mc.cores = cores)
    } else {
      parallel::mclapply(data, FUN, mc.cores = cores)
    }
  } else {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    if (showProgress && requireNamespace("pbapply", quietly = TRUE)) {
      pbapply::pblapply(data, FUN, cl = cl)
    } else {
      parallel::parLapply(cl, data, FUN)
    }
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
#' @noRd
#' 
higherSampleParallel <- function(x, species_list, genus_list, url_list, orders = orders, suborders = suborders, fullHigher = FALSE, getLink = FALSE) {
  
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
              year = sppYear,
              author = sppAuthor,
              taxa_vector = if (fullHigher) taxa_vector else NULL,
              url = if (getLink) url_list[j] else NULL
              ))
    
  }, error = function(e) {
    return(list(error = TRUE, species = species_list[j], message = e$message))
  })
}


#' Scrape taxonomic info from the Reptile Database using single core and backup
#'
#' Internal helper to extract order, suborder, family, and author info from a species page.
#'
#' @param species_list Vector of species names.
#' @param genus_list Vector of genus names.
#' @param url_list Vector of corresponding species URLs.
#' @param orders Vector with orders to be matched with taxa_vector
#' @param suborders Vector with suborders to be matched with taxa_vector
#' @param fullHigher Logical, whether to return full taxonomic string.
#' @param getLink Logical, whether to include the species page URL.
#'
#' @return dataframe with species higher taxa information with user defined backup step
#' @keywords internal
#' @noRd
#' 
higherSample <- function(species_list,
                         genus_list,
                         url_list,
                         orders = orders,
                         suborders = suborders,
                         fullHigher = FALSE,
                         getLink = FALSE,
                         backup_file = NULL,
                         checkpoint = NULL)
{
  taxa_vector_list <- c()
  order_list <- c()
  suborder_list <- c()
  family_list <- c()
  sppAuthor_list <- c()
  sppYear_list <- c()
  n_species <- length(species_list)
  
  if (is.null(checkpoint)) checkpoint <- n_species
  
  for (j in seq_along(species_list)) {
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
      
      ############################## DETECTING ERROR
      if (all(is.na(c(family, order, suborder)))) {
        message(sprintf("No higher taxa information for", species_list[j]))
      }
      ##############################
      
      taxa_vector_list <- c(taxa_vector_list, taxa_vector)
      order_list <- c(order_list, order)
      suborder_list <- c(suborder_list, suborder)
      family_list <- c(family_list, family)
      sppAuthor_list <- c(sppAuthor_list, sppAuthor)
      sppYear_list <- c(sppYear_list, sppYear)
      
      percent <- (j / n_species) * 100
      cat(sprintf("\rGetting higher taxa progress: %.1f%%", percent))
      flush.console()
      
      # back up chunk
      # Save backup every checkpoint or at the end
      if(!is.null(backup_file) &&
         (j %% checkpoint == 0 || j == n_species)){
        
        n <- length(family_list)
        backup <- data.frame(order = order_list,
                             suborder = suborder_list,
                             family = family_list,
                             genus = genus_list[1:n],
                             species = species_list[1:n],
                             author = sppAuthor_list,
                             year = sppYear_list)
        
        # Conditionally add URL if requested
        if(getLink==TRUE){
          backup$url <- url_list[1:n]
        }
        
        # Conditionally add full higher taxa vector if requested
        if(fullHigher==TRUE){
          backup$taxa_vector <- taxa_vector_list[1:n]
        }
        saveRDS(backup, file = backup_file)
        message(sprintf("Saved progress at species %d.", j, n))
      }
      
    }, error = function(e) {
      # Immediately print a concise error message for this species
      message(sprintf("Error scraping '%s': %s", species_list[j], e$message))
      # Return an error list so that you can filter or handle later
      return(list(error = TRUE, species = species_list[j], message = e$message))
    })
  }
  n <- length(family_list)
  all_vectors <- list(order = order_list[1:n], suborder = suborder_list[1:n],
                      family = family_list[1:n], genus = genus_list[1:n], species = species_list[1:n],
                      year = sppYear_list[1:n], author = sppAuthor_list[1:n],
                      taxa_vector = if(fullHigher) taxa_vector_list[1:n] else NULL,
                      url = if (getLink) url_list[1:n] else NULL)
  all_vectors <- all_vectors[!sapply(all_vectors, is.null)]
  lengths_vec <- sapply(all_vectors, length)
  expected <- lengths_vec["species"]
  if (all(lengths_vec == expected)) {
    searchResults <- as.data.frame(all_vectors, stringsAsFactors = FALSE)
  }
  else {
    message("Some vectors have different lengths! Returning list instead.")
    print(lengths_vec)
    diffs <- lengths_vec - expected
    bad <- names(diffs[diffs != 0])
    message("Mismatched vectors: ", paste(bad, collapse = ", "))
    searchResults <- all_vectors
    return(searchResults)
  }
}


#' Clean Taxonomic Species Names
#'
#' Cleans and extracts species names from noisy synonym strings, removing author names,
#' special characters, and redundant spacing.
#'
#' @param names_vec A character vector of raw synonyms name strings.
#'
#' @return A character vector of cleaned species names.
#'
#' @keywords internal
#' @noRd

clean_species_names <- function(names_vec) {
  # Build Unicode chars dynamically
  left_quote   <- intToUtf8(0x2018)
  right_quote  <- intToUtf8(0x2019)
  left_dquote  <- intToUtf8(0x201C)
  right_dquote <- intToUtf8(0x201D)
  emdash       <- intToUtf8(0x2014)
  endash       <- intToUtf8(0x2013)
  acute_e      <- intToUtf8(0x00E9)
  
  pattern <- paste0(
    "^((?:\\p{Lu}[a-z]+)\\s*(?:\\([A-Za-z]+\\))?(?:\\s+(?:",
    "[a-z]\\.|[a-z]+|\\p{Lu}[a-z]+|",
    left_quote, "?[A-Za-z]+", right_quote, "?|\\'[A-Za-z]+\\'|\\[.*?\\]|gr\\.\\s*\\w+|sp\\.\\s*nov\\.?|",
    "subsp\\.\\s*nov\\.?|var\\.\\s*\\w+|vari", acute_e, "t", acute_e, "\\.?(?:\\s*\\w+)?|",
    "aff\\.\\s*\\w+|cf\\.\\s*\\w+|\"[^\"]+\"|",
    left_dquote, "[^", right_dquote, "]+", right_dquote,
    "))+)\\s*(?:[-", endash, emdash, "]|\\(|\\b\\p{Lu}{2,}\\b|\\d{4}|\\bet al\\.\\b|\\bin\\b).*"
  )
  extracted <- sub(pattern, "\\1", names_vec, perl = TRUE)
  
  cleaned <- sub(
    "\\s+((?:\\p{Lu}{2,}|\\p{Lu}{1})\\s*(?:and|&)?\\s*)+\\b(in|et al\\.|et al|and|&)?\\b.*$",
    "",
    extracted,
    perl = TRUE
  )
  
  cleaned <- sub("^\\?\\s*", "", cleaned, perl = TRUE)
  
  cleaned <- sub(paste0("\\s*[-", endash, emdash, "-]\\s*$"), "", cleaned, perl = TRUE)
  
  # Final cleanup: collapse multiple spaces and trim
  cleaned <- gsub("\\s{2,}", " ", cleaned)
  cleaned <- trimws(gsub("\\s+", " ", cleaned))
  
  return(cleaned)
}