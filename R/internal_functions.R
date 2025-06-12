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


#' Get reptile species synonyms
#' 
#' Creates a data frame containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' 
#' @param x A data frame with columns: 'species' and 'url' (their respective Reptile Database url).
#' Could be the output of letsHerp::herpSpecies().
#' @param checkpoint An integer representing the number of species to process before saving progress to the backup file.
#' Helps prevent data loss in case the function stops unexpectedly. Backups are saved only if checkpoint is not NULL.
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them. default = *FALSE*
#' @param resume A logical value. If TRUE, takes the path to backup_file and resume sampling from the last backup file.
#' @param backup_file A character string with the path to access, read and save the backup file.
#' 
#' @returns 'getSynonyms' returns a data frame with columns: species and their respective synonyms according to the version of Reptile Database. Optionally, returns the references that mentioned each synonym.
#' 
#' @keywords internal
#' @noRd
#' 
getSynonyms <- function(x, checkpoint = NULL, resume=FALSE, backup_file = NULL, getRef=FALSE){
  
  species_list <- c()
  synonym_list <- c()
  synonymRef_list <- c()
  
  start_index <- 1
  # Load backup if resuming
  if (resume && file.exists(backup_file)) {
    x <- readRDS(backup_file)
    species <- x$species
    synonym <- x$synonym
    if ("synonymRef" %in% names(backup_file)) {
      synonymRef <- backup_file$synonymRef
    }
    
    start_index <- length(species_list) + 1
    message(sprintf("Resuming from species %d: %s", start_index, x$species[start_index]))
  }
  
  for (i in seq_along(x$species)) {
    result <- tryCatch({
      url <- rvest::read_html(httr::GET(x$url[i], httr::user_agent("Mozilla/5.0")))
      element <- rvest::html_element(url, "table") # species table
      
      # Extract synonyms
      syn <- xml2::xml_child(element, 4)
      td2 <- rvest::html_element(syn, "td:nth-child(2)")
      children <- xml2::xml_contents(td2)
      
      synonym_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
      synonym_vector <- synonym_vector[!is.na(synonym_vector) & trimws(synonym_vector) != ""]
      
      synonyms <- clean_species_names(synonym_vector)
      species <- rep(x$species[i], times = length(synonyms))
      
      species_list <- c(species_list, species)
      synonym_list <- c(synonym_list, synonyms)
      synonymRef_list <- c(synonymRef_list, synonym_vector)
      
      msg <- sprintf("Progress: %.1f%%  - %s done! ", (i / length(x$species)) * 100, x$species[i])
      cat("\r", format(msg, width = 60), sep = "")
      utils::flush.console()
      
      # Save backup every checkpoint
      if(!is.null(checkpoint)){
        if ((i %% checkpoint) == 0 || i == length(x$species)) {
          backup <- data.frame(species = species_list,
                               synonyms = synonym_list,
                               stringsAsFactors = FALSE)
          if(getRef==FALSE){
            backup$combined <- paste(backup$species, backup$synonyms, sep="_")
            uniquerec <- data.frame(unique(backup$combined))
            
            backup_uniqueSynonyms <- tidyr::separate(data=uniquerec, col="unique.backup.combined.", 
                                                     into=c("species", "synonyms"), sep="_",
                                                     convert=TRUE)
            saveRDS(backup_uniqueSynonyms, backup_file)
          }else{
            backup$synonymRef <- synonymRef_list
            saveRDS(backup, backup_file)
          }
        }else{}
      }
    }, error = function(e) {
      message(sprintf("Error scraping %s: %s", x$species[i], e$message))
      species_list <<- c(species_list, x$species[i])
      synonym_list <<- c(synonym_list, "failed")
      synonymRef_list <<- c(synonymRef_list, "failed")
    })
  }
  
  if(getRef==TRUE){
    synonymResults <- data.frame(species = species_list,
                                 synonyms = synonym_list,
                                 ref = synonymRef_list,
                                 stringsAsFactors = FALSE)
    return(synonymResults)
  }else{
    synonymResults <- data.frame(species = species_list,
                                 synonyms = synonym_list,
                                 stringsAsFactors = FALSE)
    
    synonymResults$combined <- paste((synonymResults)$species, (synonymResults)$synonyms, sep="_")
    
    uniquerec <- data.frame(unique(synonymResults$combined))
    
    uniqueSynonyms <- tidyr::separate(data=uniquerec, col="unique.synonymResults.combined.", 
                                      into=c("species", "synonym"), sep="_",
                                      convert=TRUE)
    
    cat("\nSynonyms sampling complete!\n")
    return(uniqueSynonyms)
  }
}

#' Get reptile species synonyms with parallel processing
#' 
#' Creates a data frame containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' 
#' @param x A data frame with columns: 'species' and 'url' (their respective Reptile Database url).
#' Could be the output of letsHerp::herpSpecies().
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them. default = *FALSE*
#' 
#' @returns 'getSynonymsParallel' returns a data frame with columns: species and their respective synonyms according to the version of Reptile Database. Optionally, returns the references that mentioned each synonym.
#' 
#' @keywords internal
#' @noRd
#' 
getSynonymsParallel <- function(i, x, getRef) {
  tryCatch({
    url <- rvest::read_html(httr::GET(x$url[i], httr::user_agent("Mozilla/5.0")))
    element <- rvest::html_element(url, "table")
    
    syn <- xml2::xml_child(element, 4)
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    
    synonym_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
    synonym_vector <- synonym_vector[!is.na(synonym_vector) & trimws(synonym_vector) != ""]
    
    synonyms <- clean_species_names(synonym_vector)
    species <- rep(x$species[i], times = length(synonyms))
    
    if (getRef) {
      return(data.frame(species = species, synonyms = synonyms, ref = synonym_vector, stringsAsFactors = FALSE))
    } else {
      return(data.frame(species = species, synonyms = synonyms, stringsAsFactors = FALSE))
    }
  }, error = function(e) {
    warning(sprintf("Error scraping %s: %s", x$species[i], e$message))
    if (getRef) {
      return(data.frame(species = x$species[i], synonyms = "failed", ref = "failed", stringsAsFactors = FALSE))
    } else {
      return(data.frame(species = x$species[i], synonyms = "failed", stringsAsFactors = FALSE))
    }
  })
}