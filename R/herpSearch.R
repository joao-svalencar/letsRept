#' Search The Reptile Database website (TRD)
#' 
#' @description
#' Simulates searches in The Reptile Database website and provides the data of a single species or the url for multiple species sampling by herpSpecies()\\cr
#' #' *ATTENTION:* under development, may not work yet.\\cr
#' Alternatively, for advanced search copy the link with the results from TRD and run herpSpecies()
#' 
#' @param binomial a _character_ string with the current valid binomial name of a given reptile species (e.g.: "_Apostolepis adhara_")
#' @param highertaxa a _character_ string with the current valid name of a given reptile higher taxa above genus (e.g.: "snake" or "Boidae")
#' @param genus a _character_ string with the current valid name of a given reptile genus (e.g.: "_Apostolepis_")
#' @param distribution a _character_ string with a location from which the user wants the list of species expected to occur
#'
#' @returns
#' If searching for an specific species information (e.g.: argument binomial is not NULL) - _herpSearch()_ returns the species information collected from its respective page in The Reptile Database.
#' 
#' If advanced search: returns the url to be used in herpSpecies()
#' @export
#'

herpSearch <- function(binomial=NULL, highertaxa=NULL, genus=NULL, distribution=NULL){

# block for single species search -----------------------------------------
#for binomial:
if(!is.null(binomial))
{
  base_url <- "https://reptile-database.reptarium.cz/species"
  gen <- strsplit(binomial, " ")[[1]][1]
  species <- strsplit(binomial, " ")[[1]][2]
  query <- paste0("?genus=", gen, "&species=", species)
  url <- paste0(base_url, query) #url for direct species search
  
  element <- rvest::html_element(url, "table") #get the content table for each species
  
  #pasting the content of Higher Taxa, Subspecies, Common Names, Synonyms, Distribution and Reproduction when available:
  for (i in 1:6)
  {
    row <- xml2::xml_child(element, i)
    cells <- xml2::xml_children(row)
    
    if (length(cells) >= 2) {
      title <- rvest::html_text(cells[1], trim = TRUE)
      content_raw <- rvest::html_text(cells[2], trim = TRUE)
      
      if (content_raw == "") next
      
      if (i == 4) {
        # For Synonym row: split by <br> and trim
        br_items <- cells[2] %>%
          rvest::html_nodes("br") %>%
          xml2::xml_add_sibling("marker", "<SPLIT>") # helper tag to split text
        syn_text <- rvest::html_text(cells[2], trim = TRUE)
        syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
        syn_list <- trimws(syn_list[syn_list != ""])
        
        cat("Synonyms:\n")
        for (item in syn_list) {
          cat(" -", item, "\n")
        }
        cat("\n")
      } else {
        content <- rvest::html_text(cells[2], trim = TRUE)
        cat(paste0(title,":"),"\n")
        cat(content,"\n\n")
      }
    }
  } #closes the species content printing
  return(invisible(NULL))
}else #closes the if binomial part
  { 

# block for advanced search -----------------------------------------------
  #higher taxa:
  if(!is.null(highertaxa))
    {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?taxon=", distribution, "&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
    }
  #genus:
    if(!is.null(genus))
    {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?genus=%22", genus, "%22&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
    }  
  #distribution:
    if(!is.null(distribution))
    {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?location=", distribution, "&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
    }
  return(url) 
  }
}
  