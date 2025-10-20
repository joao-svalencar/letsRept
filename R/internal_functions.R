# safeRequest -------------------------------------------------------------
safeRequest <- function(url, max_tries = 5, base_wait = 1) {
  attempt <- 1
  repeat {
    # Add random jitter to avoid hammering the server
    Sys.sleep(stats::runif(1, 0, 0.5))
    
    resp <- try(httr::GET(url, httr::user_agent("Mozilla/5.0")), silent = TRUE)
    
    if (!inherits(resp, "try-error") && httr::status_code(resp) == 200) {
      return(rvest::read_html(resp))
    }
    
    if (attempt < max_tries) {
      wait_time <- base_wait * 2^(attempt - 1)
      message(sprintf(
        "Attempt %d for %s failed (status: %s). Retrying in %ds...",
        attempt, url,
        ifelse(inherits(resp, "try-error"), "connection error", httr::status_code(resp)),
        wait_time
      ))
      Sys.sleep(wait_time)
      attempt <- attempt + 1
    } else {
      stop(sprintf("Failed to fetch %s after %d tries", url, attempt))
    }
  }
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
#' 
safeParallel <- function(data, FUN, cores = 1, showProgress = TRUE) {
  is_mac <- Sys.info()["sysname"] == "Darwin"
  
  if (.Platform$OS.type == "unix") {
    if (is_mac || cores > 1) { # macOS is unsafe with fork, or multiple cores
      cl <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      
      out <- tryCatch(
        {
          if (showProgress && requireNamespace("pbapply", quietly = TRUE)) {
            pbapply::pblapply(data, FUN, cl = cl)
          } else {
            parallel::parLapply(cl, data, FUN)
          }
        },
        interrupt = function(e) {
          message("Parallel processing was interrupted by user.")
          NULL
        },
        error = function(e) {
          message("An error occurred: ", e$message)
          NULL
        }
      )
      
    } else { # Linux, safe to use mclapply
      out <- tryCatch(
        {
          if (showProgress && requireNamespace("pbmcapply", quietly = TRUE)) {
            pbmcapply::pbmclapply(data, FUN, mc.cores = cores)
          } else {
            parallel::mclapply(data, FUN, mc.cores = cores)
          }
        },
        interrupt = function(e) {
          message("Parallel processing was interrupted by user.")
          NULL
        },
        error = function(e) {
          message("An error occurred: ", e$message)
          NULL
        }
      )
    }
    
  } else { # Windows
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    out <- tryCatch(
      {
        if (showProgress && requireNamespace("pbapply", quietly = TRUE)) {
          pbapply::pblapply(data, FUN, cl = cl)
        } else {
          parallel::parLapply(cl, data, FUN)
        }
      },
      interrupt = function(e) {
        message("Parallel processing was interrupted by user.")
        NULL
      },
      error = function(e) {
        message("An error occurred: ", e$message)
        NULL
      }
    )
  }
  return(out)
}

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
    sp_page <- safeRequest(url_list[j])
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
# higherSampleParallel_vector ---------------------------------------------
higherSampleParallel_vector <- function(binomial, orders = orders, suborders = suborders, fullHigher = FALSE, getLink = FALSE)
{ 
  base_url <- "https://reptile-database.reptarium.cz/species"
  gen <- strsplit(binomial, " ")[[1]][1]
  species <- strsplit(binomial, " ")[[1]][2]
  query <- paste0("?genus=", gen, "&species=", species)
  sppLink <- paste0(base_url, query)
  
  tryCatch({
    sp_page <- safeRequest(sppLink)
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
      genus = gen,
      species = binomial,
      year = sppYear,
      author = sppAuthor,
      taxa_vector = if (fullHigher) taxa_vector else NULL,
      url = if (getLink) sppLink else NULL
    ))
    
  }, error = function(e) {
    return(list(error = TRUE,
                species = binomial,
                message = e$message,
                url = sppLink)
    )
  })
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
    #"))+)\\s*(?:[-", endash, emdash, "]|\\(|\\b\\p{Lu}{2,}\\b|\\d{4}|\\bet al\\.\\b|\\bin\\b).*"
    "))+)\\s*(?:(?<![A-Za-z])[-", endash, emdash, "](?![A-Za-z])|\\(|\\b\\p{Lu}{2,}\\b|\\d{4}|\\bet al\\.\\b|\\bin\\b).*"
   
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
    url <- safeRequest(x$url[i])
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

# getSynonymsParallel_vector ----------------------------------------------

#' Get reptile species synonyms from vector
#' 
#' Scrapes synonyms of reptile species from The Reptile Database.
#' 
#' @param binomial A vector with \code{species} binomials.
#' @param getRef Logical. If \code{TRUE}, returns synonyms along with the references mentioning them. Default is \code{FALSE}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' 
#' @return A data frame with columns \code{species} and \code{synonyms}, optionally \code{ref} if \code{getRef = TRUE},  
#'   containing the synonyms for each species scraped from The Reptile Database.
#' 
#' @keywords internal
#' @noRd
#' 
getSynonymsParallel_vector <- function(binomial, getRef = FALSE, showProgress = TRUE) {
  base_url <- "https://reptile-database.reptarium.cz/species"
  gen <- strsplit(binomial, " ")[[1]][1]
  species <- strsplit(binomial, " ")[[1]][2]
  query <- paste0("?genus=", gen, "&species=", species)
  sppLink <- paste0(base_url, query)
  
  url <- safeRequest(sppLink)
  element <- rvest::html_element(url, "table") # species table
  
  # Extract synonyms
  syn <- xml2::xml_child(element, 4)
  td2 <- rvest::html_element(syn, "td:nth-child(2)")
  children <- xml2::xml_contents(td2)
  
  synonyms_vector <- unique(rvest::html_text(
    children[xml2::xml_name(children) == "text"],
    trim = TRUE
  ))
  synonyms_vector <- synonyms_vector[!is.na(synonyms_vector) & trimws(synonyms_vector) != ""]
  
  synonyms <- clean_species_names(synonyms_vector)
  species_list <- rep(binomial, times = length(synonyms))
  
  if (getRef) {
    synonymsResults <- data.frame(
      species = species_list,
      synonyms = synonyms,
      ref = synonyms_vector,
      stringsAsFactors = FALSE
    )
    return(synonymsResults)
  } else {
    synonymsResults <- data.frame(
      species = species_list,
      synonyms = synonyms,
      stringsAsFactors = FALSE
    )
    synonymsResults$combined <- paste(synonymsResults$species,
                                      synonymsResults$synonyms,
                                      sep = "_")
    
    uniquerec <- data.frame(unique(synonymsResults$combined))
    
    uniqueSynonyms <- tidyr::separate(
      data = uniquerec,
      col = "unique.synonymsResults.combined.",
      into = c("species", "synonyms"),
      sep = "_",
      convert = TRUE
    )
    
    if (showProgress) {
      cat("\nSynonyms sampling complete for", binomial, "\n")
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
#' @param includeAll Logical; If \code{TRUE}, include all species described since `pubDate` regardless of if it is already included in the queried species list. Default is \code{FALSE}
#' @param exact Logical. Will search queried names for exact matches only (e.g., does not retrieve "Tantilla cf. melanocephala" when searching for "Tantilla melanocephala"). Default is \code{FALSE}.
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
splitCheck <- function(spp, pubDate = NULL, verbose = TRUE, includeAll = includeAll, x, exact) {
  tryCatch({
    parts <- strsplit(spp, " ")[[1]]
    
    exact_flag <- length(parts) == 2 && tolower(parts[1]) == tolower(parts[2])
   
     if (exact_flag) {
      link <- reptAdvancedSearch(synonym = spp, verbose = verbose, exact = TRUE)
    } else {
      link <- reptAdvancedSearch(synonym = spp, verbose = verbose, exact = exact)
    }
    
    # Character link: standard HTML parsing
    if (is.character(link) && grepl("^https:", link)) {
      
      #search <- rvest::read_html(link)
      search <- safeRequest(link)
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