#' Search The Reptile Database website (TRD): Advanced
#'
#' @description
#' A short description...
#' 
#' @usage herpAdvancedSearch(highertaxa=NULL, genus=NULL, distribution=NULL)
#' 
#' @param highertaxa a _character_ string with the current valid name of a given reptile higher taxa above genus (e.g.: "snake" or "Boidae")
#' @param genus a _character_ string with the current valid name of a given reptile genus (e.g.: "_Apostolepis_")
#' @param distribution a _character_ string with a location from which the user wants the list of species expected to occur
#' 
#' @returns the url to be used in herpSpecies()
#' 
#' @export
#'
herpAdvancedSearch <- function(highertaxa=NULL, genus=NULL, distribution=NULL){

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
  #distribution:
  if(!is.null(distribution))
  {
    base_url <- "https://reptile-database.reptarium.cz/advanced_search"
    query <- paste0("?location=", distribution, "&submit=Search")
    url <- paste0(base_url, query) #url for page of species list
  }
  return(url) 
  #NEXT: IMPLEMENT SYNONYMS SEARCH AND FOR COMBINED ADVANCED ARGUMENTS
}
