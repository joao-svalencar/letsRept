##########################################################################################################
####################### function herpSynonyms by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Get reptile species synonyms
#' 
#' @description
#' creates a data frame containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' 
#' @usage herpSynonyms(x, getRef = FALSE)
#' 
#' @param x A data frame with columns: 'species' and 'url' (their respective Reptile Database url). Could be the output of letsHerp::herpSpecies().
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them. default = *FALSE*
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

herpSynonyms <- function(x, getRef=FALSE)
{

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
  
  for(i in 1:length(x$species))
  {
    url <- rvest::read_html(httr::GET(x$url[i], httr::user_agent("Mozilla/5.0")))
    element <- rvest::html_element(url, "table") #scrap species table from Reptile Database

    #synonyms
    syn <- xml2::xml_child(element, 4) #select the synonym part of the table
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    
    synonym_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
    synonym_vector <- synonym_vector[!is.na(synonym_vector) & trimws(synonym_vector) != ""]
    
    synonyms <- clean_species_names(synonym_vector)
    
    cat(sprintf(
      "%s done!\nProgress: %.1f%%\n\n",
      x$species[i],
      (i / length(x$species)) * 100
    ))
    utils::flush.console()
    
    species <- c(rep(x$species[i], times=length(synonyms)))
    
    species_list <- c(species_list, species)
    synonym_list <- c(synonym_list, synonyms)
    synonymRef_list <- c(synonymRef_list, synonym_vector)
  
  }#loop for ends here

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
                                     convert=TRUE) #funcao de separacao
      return(uniqueSynonyms)
    }
}
