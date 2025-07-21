#' Search The Reptile Database website (RDB): Advanced
#'
#' @description
#' Creates a search URL for retrieving species lists from RDB based on multiple filters.
#' This URL is primarily used by \code{\link{reptSpecies}}, but can also be used manually for advanced queries.
#' 
#' If a synonym is provided and can be unambiguously matched to a valid species, the function also prints detailed information for that species.
#' 
#' @param higher Character string. A higher-level reptile taxon above genus (e.g., \code{"snakes"} or \code{"Boidae"}).
#' @param genus Character string. The current valid name of a reptile genus (e.g., \code{"Apostolepis"}).
#' @param year Character string. Filters the search by year of species description (e.g., \code{"2025"}).
#' @param common_name Character string. A common name potentially linked to a species or genus (e.g., \code{"tree boa"}).
#' @param synonym Character string. A name potentially regarded as a synonym of a valid taxon (e.g., \code{"Boa diviniloqua"}).
#' @param location Character string. A country or region name used to list species expected to occur there.
#' @param verbose Logical. To be passed to \code{reptSpecies()} in the case of a provided synonym corresponds unambiguously to a valid species.
#' If \code{TRUE}, prints status messages and species information in the console. Default is \code{TRUE}.
#' @param exact Logical. To return outputs that matches exactly the searched term (e.g., avoid returning genus "Boaedon" when searching for "Boa"). Default is \code{FALSE}.
#'
#' @return A character string containing the URL to be used in \code{\link{reptSpecies}}.
#' 
#' If a provided synonym corresponds unambiguously to a valid species, the function also prints species information retrieved from RDB to the console.
#' 
#' @note
#' 
#' The argument \code{exact} does not work properly for searches using logical arguments (e.g. \code{AND/OR}).
#' If you want to force an exact match (e.g., \code{"Boa"} as a phrase) with multiple terms (e.g., \code{"Boa OR Apostolepis"}),
#' you must manually include quotes in the input string, e.g., \code{"\"Boa\" OR Apostolepis"}.
#' 
#' Logical operators (e.g., \code{OR}, \code{AND}) are supported and will be properly formatted in the search.
#' To exclude terms, use a leading minus sign (e.g., \code{higher = "-snakes"}) following RDB's query syntax, instead of using \code{NOT}.
#' 
#' When a synonym is matched to a single valid species, the function will also display the species' full information as a side effect.
#' 
#' @examples
#' \donttest{
#' reptAdvancedSearch(higher = "snakes", year = "2010", location = "Brazil")
#' reptAdvancedSearch(year = "2010 OR 2011 OR 2012")
#' reptAdvancedSearch(genus = "Apostolepis OR \"Boa\" OR Atractus") #quotes "Boa"
#' }
#' @export
#'
reptAdvancedSearch <- function(higher = NULL,
                               genus = NULL,
                               year = NULL,
                               common_name = NULL,
                               synonym = NULL,
                               location = NULL,
                               verbose = TRUE,
                               exact = FALSE) {
  
  # Check if all arguments are NULL
  if (all(sapply(list(higher, genus, year, common_name, synonym, location), is.null))) {
    stop("\n No query parameters provided. Please supply at least one.\n")
    return(NULL)
  }
  
  base_url <- "https://reptile-database.reptarium.cz/advanced_search"
  
  quote_if_simple <- function(x, exact = TRUE) {
    if (grepl("\\b(OR|AND|NOT)\\b", x, ignore.case = TRUE)) {
      if (exact) {
        warning("Exact matching not applicable to logical 'AND/OR/NOT' queries for multiple terms.")
      }
      return(utils::URLencode(gsub(" ", "+", x), reserved = FALSE))
    } else {
      if (exact) x <- paste0('"', x, '"')
      return(utils::URLencode(x, reserved = FALSE))
    }
  }
  
  # Build list of query parameters based on non-NULL arguments
  params <- list()
  
  if (!is.null(higher))       params$taxon          <- quote_if_simple(higher, exact)
  if (!is.null(genus))        params$genus          <- quote_if_simple(genus, exact)
  if (!is.null(year))         params$year           <- quote_if_simple(year, exact)
  if (!is.null(common_name))  params$common_name    <- quote_if_simple(common_name, exact)
  if (!is.null(synonym))      params$common_name    <- quote_if_simple(synonym, exact)
  if (!is.null(location))     params$location       <- quote_if_simple(location, exact)
  
  # Always include the submit flag
  params$submit <- "Search"
  
  # Collapse the parameters into a query string
  query <- paste0("?", paste0(names(params), "=", params, collapse = "&"))
  
  # Final URL
  url <- paste0(base_url, query)
  
  # implement link test:
  test <- rvest::read_html(url)
  title_node <- rvest::html_element(test, "h1")
  title_text <- rvest::html_text(title_node, trim = TRUE)
  
  if (grepl("^Search results", title_text)) {
    # This is a multi-species search results page
    ul_element <- rvest::html_element(test, "#content > p:nth-child(5)")
    msg <- rvest::html_text(ul_element)
    
    if (grepl("^Species found:", msg)) {
      if(verbose) message(msg, "\nProceed to reptSpecies() with the returned link\n")
      return(url)
    } else if (grepl("No species were found", msg)) {
      warning("No species were found. Please verify the search arguments.\n")
      return("not_found")
    } else {
      warning("Unexpected content in a search results page.\n")
      return(invisible(NULL))
    }
    
  } else {
    # Presumably this is a direct species page
    binomial <- rvest::html_text(rvest::html_element(title_node, "em"), trim = TRUE)
    if(verbose) message("Searched binomial is currently:\n", binomial, "\n")
    search <- reptSearch(binomial = binomial, verbose = FALSE)
    return(search)
  }
}
