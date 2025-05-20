##########################################################################################################
####################### function herpSynonyms by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Get reptile species synonyms
#'
#' @description
#' creates a dataframe containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#'
#' @param x A dataframe with columns: 'species' and 'url' (their respective Reptile Database url). Could be the output of letsHerp::herpSpecies()
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them 
#'
#' @returns '_herpSynonyms_' returns a dataframe with columns: species and their respective synonyms according to the version of Reptile Database.
#' 
#' @references 
#' Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database, http://www.reptile-database.org
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @usage herpSynonyms(x, getRef = FALSE)
#' @export
#'

herpSynonyms <- function(x, getRef=FALSE)
{

# creates clean_names function: -------------------------------------------
  # Build Unicode chars dynamically
  left_quote  <- intToUtf8(0x2018)
  right_quote <- intToUtf8(0x2019)
  left_dquote <- intToUtf8(0x201C)
  right_dquote <- intToUtf8(0x201D)
  emdash      <- intToUtf8(0x2014)
  endash      <- intToUtf8(0x2013)
  acute_e     <- intToUtf8(0x00E9)
  
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
    extracted <- sub(pattern, "\\1", names_vec, perl=TRUE)
    
    cleaned <- sub(
      "\\s+((?:\\p{Lu}{2,}|\\p{Lu}{1})\\s*(?:and|&)?\\s*)+\\b(in|et al\\.|et al|and|&)?\\b.*$",
      "",
      extracted,
      perl = TRUE
    )
    
    cleaned <- sub("^\\?\\s*", "", cleaned, perl = TRUE)
    
    cleaned <- sub(paste0("\\s*[-", endash, emdash, "-]\\s*$"), "", cleaned, perl = TRUE)
    
    return(cleaned)
  }

# end of function ---------------------------------------------------------

  species_list <- c()
  synonym_list <- c()
  synonymRef_list <- c()
  
  for(i in 1:length(x$species))
  {
    # try_get_html <- function(url, tries = 3) {
    #   for (attempt in 1:tries) {
    #     try({
    #       response <- httr::GET(url, httr::user_agent("Mozilla/5.0"), timeout(30))
    #       return(rvest::read_html(response))
    #     }, silent = TRUE)
    #     Sys.sleep(runif(1, 1, 3))  # slightly longer pause after failure
    #   }
    #   warning(sprintf("Failed to fetch: %s after %d tries", url, tries))
    #   return(NULL)
    # }
    # 
    # #add random sleep time
    # Sys.sleep(stats::runif(1, min = 0.3, max = 1)) # random sleep time
    # 
    # url <- try_get_html(x$url[i])
    # if (!is.null(url)) {
    #   element <- rvest::html_element(url, "table")
    # }
    
    url <- rvest::read_html(httr::GET(x$url[i], httr::user_agent("Mozilla/5.0")))
    element <- rvest::html_element(url, "table") #scrap species table from Reptile Database

    #synonyms
    syn <- xml2::xml_child(element, 4) #select the synonym part of the table
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    synonym_vector <- unique(children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE))
    
    synonyms <- clean_species_names(synonym_vector)
    
    cat(paste("Species number",paste0(i,"",":"), "\n", x$species[i],"\n", "Done!", "\n", "\n"))
    species <- c(rep(x$species[i], times=length(synonyms)))
    
    species_list <- c(species_list, species)
    synonym_list <- c(synonym_list, synonyms)
    synonymRef_list <- c(synonymRef_list, synonym_vector)
  
}   #loop for ends here
    synonymResults <- data.frame(species = species_list,
                                 synonyms = synonym_list,
                                 stringsAsFactors = FALSE)
    if(getRef==TRUE){
      synonymResults$ref <- synonymRef_list
    }
  return(synonymResults)
}
