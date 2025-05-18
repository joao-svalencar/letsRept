#' Search The Reptile Database website (TRD): Advanced
#'
#' @description
#' Searches The Reptile Database website and provides the data of a single species or the url for multiple species sampling by herpSpecies()\\cr
#' #' *ATTENTION:* under development, may not work yet.\\cr
#' Alternatively, for advanced search copy the link with the results from TRD and run herpSpecies()
#' 
#' @usage herpAdvancedSearch(highertaxa=NULL, genus=NULL, synonym=NULL, distribution=NULL)
#' 
#' @param highertaxa A character string with the current valid name of a given reptile higher taxa above genus (e.g.: "snake" or "Boidae")
#' @param genus A character string with the current valid name of a given reptile genus (e.g.: "_Apostolepis_")
#' @param synonym A character string with name potentially regarded as a synonym of a given reptile genus (e.g.: "_Boa diviniloqua_")
#' @param distribution A character string with a location from which the user wants the list of species expected to occur
#' 
#' @returns the url to be used in herpSpecies()
#' 
#' @export
#'
herpAdvancedSearch <- function(highertaxa=NULL, genus=NULL, synonym=NULL, distribution=NULL){

  #higher taxa:
  if(!is.null(highertaxa))
  {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?taxon=", highertaxa, "&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
  }
  #genus:
  if(!is.null(genus))
  {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?genus=%22", genus, "%22&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
  }
  #synonyms:
  if(!is.null(synonym))
  {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?common_name=", sub(" ", "+", synonym), "&submit=Search")
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
  #NEXT: IMPLEMENT TEST OF LINK VIABILITY AND SEARCH OF COMBINED ADVANCED ARGUMENTS
}
