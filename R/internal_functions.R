# match_taxon -------------------------------------------------------------

#' Match higher taxonomic ranks
#'
#' Helper for \code{reptSpecies()}. Matches known taxonomic ranks (e.g., orders or suborders) from a character vector of taxonomic information.
#'
#' @param taxa_vector A character vector containing taxonomic strings.
#' @param rank_list A character vector of known rank names to match (e.g., \code{c("Squamata", "Serpentes")}).
#'
#' @return A single character string with matched ranks, comma-separated. Returns \code{NA} if none are found.
#' 
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
  return(sorted_matches[1])
}

# safeParallel ------------------------------------------------------------

#' Safe parallel execution
#'
#' Helper for functions like \code{reptSpecies()} and \code{reptSynonyms()}. Selects a safe parallel backend depending on the user's OS and optionally shows progress bars.
#'
#' @param data A list or vector of items to process in parallel.
#' @param FUN A function to be applied to each element of \code{data}.
#' @param cores Integer. Number of processor cores to use.
#' @param showProgress Logical. Whether to display a progress bar if supported. Default is \code{TRUE}.
#'
#' @return A list containing the results of applying \code{FUN} to each element of \code{data}.
#' 
#' @keywords internal
#' @noRd
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

# higherSampleParallel ----------------------------------------------------

#' Extract taxonomic info for one species (parallel worker)
#'
#' Internal function used by \code{reptSpecies()} to scrape taxonomic data for multiple species in parallel.
#' This function handles a single species per call and is designed to be mapped over a vector of species using parallel execution.
#'
#' @param x A species name to match.
#' @param species_list Character vector of species names.
#' @param genus_list Character vector of genus names.
#' @param url_list Character vector of corresponding species URLs.
#' @param orders Character vector of order names to match.
#' @param suborders Character vector of suborder names to match.
#' @param fullHigher Logical. Whether to return the full higher taxonomic string.
#' @param getLink Logical. Whether to include the species URL.
#'
#' @return A named list with taxonomic information (e.g., order, suborder, family, genus, species, author, year).
#' Returns a list with an error message on failure.
#'
#' @note Called once per species as part of a parallelized taxonomic sampling routine. Not intended for standalone use.
#' 
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
    return(list(error = TRUE,
                species = species_list[j],
                message = e$message,
                url = url_list[j]))
  })
}

# higherSample ------------------------------------------------------------


#' Extract taxonomic info from the Reptile Database (single-core with backup)
#'
#' Internal function used by \code{reptSpecies()} when \code{cores = 1}. Loops over species pages and extracts
#' taxonomic information (order, suborder, family, author, etc.). Supports progress printing and checkpoint-based backups.
#'
#' @param species_list Character vector of species names.
#' @param genus_list Character vector of genus names.
#' @param url_list Character vector of corresponding species URLs.
#' @param orders Character vector of order names to match.
#' @param suborders Character vector of suborder names to match.
#' @param fullHigher Logical. Whether to return the full higher taxonomic string (default is \code{FALSE}).
#' @param getLink Logical. Whether to include the species URL in the output (default is \code{FALSE}).
#' @param backup_file Optional. File path to save periodic backup using \code{saveRDS()}.
#' @param checkpoint Optional. Integer specifying how often (in number of species) to save backup. If \code{NULL}, saves only once at the end.
#' @param showProgress Logical. if \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#'
#' @return A data frame containing the scraped taxonomic information. If vector lengths mismatch, returns a named list instead.
#'
#' @note Intended for internal use only. Called by \code{reptSpecies()} when not using parallelization.
#'
#' @keywords internal
#' @noRd
higherSample <- function(species_list,
                         genus_list,
                         url_list,
                         orders = orders,
                         suborders = suborders,
                         fullHigher = FALSE,
                         getLink = FALSE,
                         backup_file = NULL,
                         checkpoint = NULL,
                         showProgress = TRUE)
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
      
      if(showProgress == TRUE){
      percent <- (j / n_species) * 100
      cat(sprintf("\rGetting higher taxa progress: %.1f%%", percent))
      flush.console()
      }
      
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
      return(list(error = TRUE,
                  species = species_list[j],
                  message = e$message,
                  url=url_list[j]))
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

# clean_species_names -----------------------------------------------------

#' Clean taxonomic species names from synonym strings
#'
#' Helper function for \code{reptSynonyms()}. Extracts and cleans species names from raw synonym strings,
#' removing authorship, special characters, Unicode quotes, and redundant elements.
#'
#' @param names_vec Character vector of raw synonym name strings.
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

# getSynonymsParallel -----------------------------------------------------

#' Get reptile species synonyms with parallel processing
#' 
#' Internal helper to retrieve synonyms for reptile species from The Reptile Database.
#' Designed to be called in parallel over a data frame of species and URLs.
#' 
#' @param i Integer index for the species to process (used internally by parallel loops).
#' @param x Data frame with columns \code{species} and \code{url} (e.g., output from \code{reptSpecies()}).
#' @param getRef Logical; if \code{TRUE}, returns synonyms along with their source references. Default \code{FALSE}.
#' 
#' @return A data frame with columns \code{species}, \code{synonyms}, and optionally \code{ref}.
#'         Returns a row with \code{"failed"} if an error occurs.
#' 
#' @keywords internal
#' @noRd
getSynonymsParallel <- function(i, x, getRef) {
  tryCatch({
    url <- rvest::read_html(httr::GET(x$url[i], httr::user_agent("Mozilla/5.0")))
    element <- rvest::html_element(url, "table")
    
    syn <- xml2::xml_child(element, 4)
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    
    synonyms_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
    synonyms_vector <- synonyms_vector[!is.na(synonyms_vector) & trimws(synonyms_vector) != ""]
    
    synonyms <- clean_species_names(synonyms_vector)
    species <- rep(x$species[i], times = length(synonyms))
    
    if (getRef) {
      return(data.frame(species = species, synonyms = synonyms, ref = synonyms_vector, stringsAsFactors = FALSE))
    } else {
      return(data.frame(species = species, synonyms = synonyms, stringsAsFactors = FALSE))
    }
  }, error = function(e) {
    warning(sprintf("Error scraping %s: %s", x$species[i], e$message))
    if (getRef) {
      df <- data.frame(species = x$species[i], synonyms = "failed", ref = "failed", stringsAsFactors = FALSE)
      return(df)
    } else {
      df <- data.frame(species = x$species[i], synonyms = "failed", stringsAsFactors = FALSE)
      return(df)
    }
  })
}

# getSynonyms -------------------------------------------------------------

#' Get reptile species synonyms
#' 
#' Scrapes synonyms of reptile species from The Reptile Database.
#' 
#' @param x A data frame with columns \code{species} and \code{url} (the respective Reptile Database URLs).  
#'   Typically, the output of \code{letsRept::reptSpecies()}.
#' @param checkpoint Integer specifying how many species to process before saving progress to \code{backup_file}.  
#'   Helps avoid data loss if the function stops unexpectedly. Backups are saved only if \code{checkpoint} is not \code{NULL}.
#' @param resume Logical. If \code{TRUE}, resumes processing from a previous backup saved at \code{backup_file}.
#' @param backup_file Character string path to save or load the backup file.
#' @param getRef Logical. If \code{TRUE}, returns synonyms along with the references mentioning them. Default is \code{FALSE}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' 
#' @return A data frame with columns \code{species} and \code{synonyms}, optionally \code{ref} if \code{getRef = TRUE},  
#'   containing the synonyms for each species scraped from The Reptile Database.
#' 
#' @keywords internal
#' @noRd
#' 
getSynonyms <- function(x, checkpoint = NULL, resume=FALSE, backup_file = NULL, getRef=FALSE, showProgress=TRUE){
  
  species_list <- c()
  synonyms_list <- c()
  synonymsRef_list <- c()
  
  start_index <- 1
  # Load backup if resuming
  if (resume && file.exists(backup_file)) {
    x <- readRDS(backup_file)
    species <- x$species
    synonyms <- x$synonyms
    if ("synonymRef" %in% names(backup_file)) {
      synonymsRef <- backup_file$synonymsRef
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
      
      synonyms_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
      synonyms_vector <- synonyms_vector[!is.na(synonyms_vector) & trimws(synonyms_vector) != ""]
      
      synonyms <- clean_species_names(synonyms_vector)
      species <- rep(x$species[i], times = length(synonyms))
      
      species_list <- c(species_list, species)
      synonyms_list <- c(synonyms_list, synonyms)
      synonymsRef_list <- c(synonymsRef_list, synonyms_vector)
      
      if(showProgress == TRUE){
      msg <- sprintf("Progress: %.1f%%  - %s done! ", (i / length(x$species)) * 100, x$species[i])
      cat("\r", format(msg, width = 60), sep = "")
      utils::flush.console()
      }
      # Save backup every checkpoint
      if(!is.null(checkpoint)){
        if ((i %% checkpoint) == 0 || i == length(x$species)) {
          backup <- data.frame(species = species_list,
                               synonyms = synonyms_list,
                               stringsAsFactors = FALSE)
          if(getRef==FALSE){
            backup$combined <- paste(backup$species, backup$synonyms, sep="_")
            uniquerec <- data.frame(unique(backup$combined))
            
            backup_uniqueSynonyms <- tidyr::separate(data=uniquerec, col="unique.backup.combined.", 
                                                     into=c("species", "synonyms"), sep="_",
                                                     convert=TRUE)
            saveRDS(backup_uniqueSynonyms, backup_file)
          }else{
            backup$synonymsRef <- synonymsRef_list
            saveRDS(backup, backup_file)
          }
        }else{}
      }
    }, error = function(e) {
      message(sprintf("Error scraping %s: %s", x$species[i], e$message))
      species_list <<- c(species_list, x$species[i])
      synonyms_list <<- c(synonyms_list, "failed")
      synonymsRef_list <<- c(synonymsRef_list, "failed")
    })
  }
  
  if(getRef==TRUE){
    synonymsResults <- data.frame(species = species_list,
                                  synonyms = synonyms_list,
                                  ref = synonymsRef_list,
                                  stringsAsFactors = FALSE)
    return(synonymsResults)
  }else{
    synonymsResults <- data.frame(species = species_list,
                                  synonyms = synonyms_list,
                                  stringsAsFactors = FALSE)
    
    synonymsResults$combined <- paste((synonymsResults)$species, (synonymsResults)$synonyms, sep="_")
    
    uniquerec <- data.frame(unique(synonymsResults$combined))
    
    uniqueSynonyms <- tidyr::separate(data=uniquerec, col="unique.synonymsResults.combined.", 
                                      into=c("species", "synonyms"), sep="_",
                                      convert=TRUE)
    
    if(showProgress == TRUE){
    cat("\nSynonyms sampling complete!\n")
    }
    return(uniqueSynonyms)
  }
}

# split check -------------------------------------------------------------

#' Check if a species name might represent a split taxon
#'
#' @description
#' Internal helper function used in \code{reptSplitCheck()} to assess whether a
#' species name potentially corresponds to multiple recently described taxa.
#' It scrapes The Reptile Database and checks publication years against a user-defined threshold.
#'
#' @param spp A character string with a species name to check.
#' @param pubDate An optional integer year (e.g., 2020) to compare against taxon publication years.
#' Species published from this year onward may indicate a recent split.
#' @param verbose Logical; if \code{TRUE}, messages will be printed when an error occurs. Default is \code{TRUE}.
#'
#' @return A data frame with three columns:
#' \itemize{
#'   \item \code{query} – the input species name
#'   \item \code{RDB} – one or more matched species (or \code{NA} if none)
#'   \item \code{status} – one of \code{"check_split"}, \code{"up_to_date"}, \code{"not_found"}, or \code{"failed"}
#' }
#'
#' @keywords internal
#' @noRd
splitCheck <- function(spp, pubDate = NULL, verbose = TRUE, includeAll = includeAll, x) {
  tryCatch({
    link <- reptAdvancedSearch(synonym = spp, verbose = verbose)
    
    # Character link: standard HTML parsing
    if (is.character(link) && grepl("^https:", link)) {
      search <- rvest::read_html(link)
      title_node <- rvest::html_element(search, "h1")
      title_text <- rvest::html_text(title_node, trim = TRUE)
      
      if (grepl("^Search results", title_text)) {
        ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
        if (length(ul_element) == 0) return(NULL)
        
        li_nodes <- xml2::xml_children(ul_element[[1]])
        species <- sapply(li_nodes, function(node) {
          rvest::html_text(rvest::html_element(xml2::xml_child(node, 1), "em"), trim = TRUE)
        })
        
        years <- sapply(li_nodes, function(node) {
          as.integer(stringr::str_extract(rvest::html_text(node), "(?<!\\d)\\d{4}(?!\\d)"))
        })
        
        syn_df <- data.frame(species, years, stringsAsFactors = FALSE)
        
        if(includeAll == TRUE){
          if(!any(syn_df$years >= pubDate, na.rm = TRUE) && spp %in% syn_df$species){
            return(data.frame(query = spp, RDB = spp, status = "up_to_date", stringsAsFactors = FALSE))
          }
          
          if(any(syn_df$years >= pubDate, na.rm = TRUE)) {
            matched_species <- paste(syn_df$species[syn_df$years >= pubDate], collapse = "; ")
            return(data.frame(query = spp, RDB = matched_species, status = "check_split", stringsAsFactors = FALSE))
            } 
          }else{
            
          syn_df_filter <- syn_df[!syn_df$species %in% setdiff(x, spp), ]
  
          if(!any(syn_df_filter$years >= pubDate, na.rm = TRUE) && spp %in% syn_df$species){
            return(data.frame(query = spp, RDB = spp, status = "up_to_date", stringsAsFactors = FALSE))
          }
          
          if(any(syn_df_filter$years >= pubDate, na.rm = TRUE)) {
            matched_species <- paste(syn_df_filter$species[syn_df_filter$years >= pubDate], collapse = "; ")
            return(data.frame(query = spp, RDB = matched_species, status = "check_split", stringsAsFactors = FALSE))
          }
        }
      }
      
    } else if (is.list(link) && !is.null(link$url)) {
      search <- rvest::read_html(link$url)
      title_node <- rvest::html_element(search, "h1")
      em_species <- rvest::html_text(rvest::html_element(title_node, "em"), trim = TRUE)
      
      if (spp == em_species) {
        return(data.frame(query = spp, RDB = em_species, status = "up_to_date", stringsAsFactors = FALSE))
      }
    }
    
    # Fallback
    data.frame(query = spp, RDB = spp, status = "not_found", stringsAsFactors = FALSE)
    
  }, error = function(e) {
    if (verbose) message(sprintf("Error for '%s': %s", spp, conditionMessage(e)))
    data.frame(query = spp, RDB = NA, status = "failed", stringsAsFactors = FALSE)
  })
}


# End of internal functions -----------------------------------------------
