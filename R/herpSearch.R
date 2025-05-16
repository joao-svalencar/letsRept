#' Search The Reptile Database website (TRD)
#' 
#' @description
#' ATTENTION: under development, may not work yet. Alternatively, for advanced search copy the link with the results from TRD and run herpSpecies()
#' simulates searches in The Reptile Database website and provides the data of a single species or the url for multiple species sampling by herpSpecies()
#' 
#' @param binomial a _character_ string with the current valid binomial name of a given reptile species (e.g.: "Apostolepis adhara")
#' @param highertaxa a _character_ string with the current valid name of a given reptile higher taxa above genus (e.g.: "snake" or "Boidae")
#' @param genus a _character_ string with the current valid name of a given reptile genus (e.g.: "Apostolepis")
#' @param distribution a _character_ string with a location from which the user wants the list of species expected to occur
#'
#' @returns
#' If a search for an specific species information (e.g.: argument binomial is not NULL): returns the species information collected from its respective page in The Reptile Database
#' If advanced search: returns the url to be used in herpSpecies()
#' @export
#'

herpSearch <- function(binomial=NULL, highertaxa=NULL, genus=NULL, distribution=NULL){

  # block for single species search -----------------------------------------
  #for binomial:
  if(!is.null(binomial))
  {
    base_url <- "https://reptile-database.reptarium.cz/species"
    genus <- strsplit(binomial, " ")[[1]][1]
    species <- strsplit(binomial, " ")[[1]][2]
    query <- paste0("?genus=", genus, "&species=", species)
    url <- paste0(base_url, query) #url for direct species search
    
    #paste the scrapping for one species code
  }

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

boa<-herpSearch(binomial= "Boa constrictor")

url <- rvest::read_html(boa)
element <- rvest::html_element(url, "table") #scrap species table from Reptile Database
syn <- xml2::xml_child(element, 4) #select the synonym part of the table
td2 <- rvest::html_element(syn, "td:nth-child(2)")
children <- xml2::xml_contents(td2)
synonym_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(x) {
  if (length(x) >= 3 && x[2] %in% c("aff.", "cf", "gr.")) {
    paste(x[1:3], collapse = " ")
  } else {
    paste(x[1:2], collapse = " ")
  }
}))
