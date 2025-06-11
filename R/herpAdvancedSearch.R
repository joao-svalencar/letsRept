##########################################################################################################
################### function herpAdvancedSearch by:  JP VIEIRA-ALENCAR  ##################################
##########################################################################################################

#' Search The Reptile Database website (TRD): Advanced
#'
#' @description
#' Creates the link for multiple species sampling by herpSpecies()
#' 
#' @usage herpAdvancedSearch(higher=NULL,
#'                           genus=NULL,
#'                           year=NULL,
#'                           common_name=NULL,
#'                           synonym=NULL,
#'                           location=NULL)
#' 
#' @param higher A character string with the current valid name of a given reptile higher taxa above genus (e.g.: "snake" or "Boidae")
#' @param genus A character string with the current valid name of a given reptile genus (e.g.: "_Apostolepis_")
#' @param year A character string to be used as a filter for the year of description of the searched species (e.g.: "2025")
#' @param common_name A character string with name potentially regarded as a synonym of a given reptile genus (e.g.: "_Boa diviniloqua_")
#' @param synonym A character string with name potentially regarded as a synonym of a given reptile genus (e.g.: "_Boa diviniloqua_")
#' @param location A character string with a location from which the user wants the list of species expected to occur
#' 
#' @returns the url to be used in herpSpecies()
#' 
#' @examples
#' herpAdvancedSearch(higher = "snakes", year = "2010", location = "Brazil")
#' herpAdvancedSearch(higher = "Sauria", location = "Argentina")
#' herpAdvancedSearch(location = "Brazil", genus = "Atractus OR Apostolepis")
#' herpAdvancedSearch(year = "2010 OR 2011 OR 2012")
#' 
#' @export
#'
herpAdvancedSearch <- function(higher = NULL, genus = NULL, year = NULL, common_name = NULL, synonym = NULL, location = NULL) {
  
  # Check if all arguments are NULL
  if (all(sapply(list(higher, genus, year, common_name, synonym, location), is.null))) {
    cat("\n No query parameters provided. Please supply at least one.\n")
    return(NULL)
  }
  
  base_url <- "https://reptile-database.reptarium.cz/advanced_search"
  
  quote_if_simple <- function(x) {
    if (grepl("\\b(OR|AND|NOT)\\b", x, ignore.case = TRUE)) {
      # Logical query: replace spaces with +
      gsub(" ", "+", x)
    } else {
      # Exact match: wrap in quotes, then encode
      utils::URLencode(paste0('"', x, '"'), reserved = FALSE)
    }
  }
  
  encode_param <- function(x) utils::URLencode(x, reserved = FALSE)
  
  # Build list of query parameters based on non-NULL arguments
  params <- list()
  
  if (!is.null(higher))       params$taxon          <- encode_param(quote_if_simple(higher))
  if (!is.null(genus))        params$genus          <- encode_param(quote_if_simple(genus))
  if (!is.null(year))         params$year           <- encode_param(quote_if_simple(year))
  if (!is.null(common_name))  params$common_name    <- encode_param(quote_if_simple(common_name))
  if (!is.null(synonym))      params$common_name    <- encode_param(quote_if_simple(synonym))
  if (!is.null(location))     params$location       <- encode_param(quote_if_simple(location))
  
  # Always include the submit flag
  params$submit <- "Search"
  
  # Collapse the parameters into a query string
  query <- paste0("?", paste0(names(params), "=", params, collapse = "&"))
  
  # Final URL
  url <- paste0(base_url, query)
  
  # implement link test:
  test <- rvest::read_html(url)#
  ul_element <- rvest::html_elements(test, "#content > p:nth-child(5)")
  msg <- rvest::html_text(ul_element[[1]])
  
  if (grepl("Find more photos by Google images search:", msg)){
    title <- rvest::html_element(test, "h1")
    binomial <- rvest::html_text(rvest::html_element(title, "em"), trim = TRUE)
    herpSearch(binomial=binomial)
  }else if (grepl("^Species found:", msg)) {
    cat(msg, "\nProceed to herpSpecies() with the returned link")
    return(url)
  } else if (grepl("No species were found", msg)) {
    cat("No species were found. Please verify the search arguments.\n")
    return(NULL)
  } else {
    cat("Unexpected page content. Investigate manually.\n")
    return(NULL)
  }
}
