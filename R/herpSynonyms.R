##########################################################################################################
####################### function herpSynonyms by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Get reptile species synonyms
#' 
#' @description
#' creates a data frame containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' 
#' @usage herpSynonyms(x, 
#'                     checkpoint = NULL,
#'                     resume=FALSE,
#'                     backup_file = NULL,
#'                     getRef = FALSE)
#' 
#' @param x A data frame with columns: 'species' and 'url' (their respective Reptile Database url).
#' Could be the output of letsHerp::herpSpecies().
#' @param checkpoint An integer representing the number of species to process before saving progress to the backup file.
#' Helps prevent data loss in case the function stops unexpectedly. Backups are saved only if checkpoint is not NULL.
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them. default = *FALSE*
#' @param resume A logical value. If TRUE, takes the path to backup_file and resume sampling from the last backup file.
#' @param backup_file A character string with the path to access, read and save the backup file.
#' 
#' @returns 'herpSynonyms' returns a data frame with columns: species and their respective synonyms according to the version of Reptile Database. Optionally, returns the references that mentioned each synonym.
#' 
#' @references 
#' Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database, http://www.reptile-database.org
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @examples
#' boaLink <- herpAdvancedSearch(genus = "Boa") #creates advanced search link
#' boa <- herpSpecies(boaLink, getLink=TRUE, taxonomicInfo = FALSE)
#' boa_syn <- herpSynonyms(boa, getRef = TRUE) #synonyms with respective references
#' boa_syn <- herpSynonyms(boa, getRef = FALSE) #only synonyms
#' 
#' @export
#'

herpSynonyms <- function(x, checkpoint = NULL, resume=FALSE, backup_file = NULL, getRef=FALSE)
{
  if (is.null(backup_file) && !is.null(checkpoint) && checkpoint < length(x$species)) {
    stop("You must provide a valid backup_file path if checkpoint is smaller than the number of species.")
  }else 
    if(!is.null(backup_file) && !grepl(".rds", backup_file)){
      stop("Backup file path must end with 'filename.rds'")
    }
  
# creates clean_names function: -------------------------------------------
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
  clean_species_names <- function(names_vec) {
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
# end of cleaning function ------------------------------------------------

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
