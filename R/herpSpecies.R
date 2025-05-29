##########################################################################################################
######################## function herpSpecies by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Reptile species summary
#'
#' @description 
#' Creates a data frame containing higher taxa information for a list of reptile species based on a Reptile Database advanced search optionally with their respective url.
#' 
#' @usage herpSpecies(url,
#'                    dataList = NULL, 
#'                    taxonomicInfo=FALSE, 
#'                    fullHigher=FALSE, 
#'                    getLink=FALSE, 
#'                    checkpoint=NULL,
#'                    backup_file = NULL,
#'                    cores = (parallel::detectCores()-1))
#'                    
#' @param url A character string with the url from an advanced search in Reptile Database website or from letsHerp::herpAdvancedSearch.
#' @param dataList A data frame with columns: species and url, only for sampling taxonomicInfo from already sampled species links.
#' @param taxonomicInfo A logical value indicating if user wants full species taxonomic information (specifically: Order, Suborder, Family, Genus, species author and description year) for each species. default = *TRUE*
#' @param fullHigher A logical value indicating if user wants the full higher taxa information (including e.g.: subfamily) for each species, as available in The Reptile Database website (e.g. single character string). default = *FALSE*. OBS.: Requires taxonomicInfo = TRUE
#' @param getLink A logical value indicating if user wants the url that provides access to each species information (e.g: to use with herpSynonyms()). default = *TRUE*
#' @param cores An integer representing how many cores to use during parallel sampling. default is 1 less than all available cores
#'
#' @return if taxonomicInfo = FALSE (default), the function returns a vector with the list of species
#' 
#' if taxonomicInfo = TRUE, the function returns a data frame with columns: order, suborder (e.g.: Sauria or Serpentes only; when available), family, genus, species, author and year
#' 
#' Optionally, the function might return a data frame with a column with the full higher taxa information as reported in The Reptile Database, and the species respective url (necessary if looking for synonyms afterwards)
#'
#' 
#' @export
#'

herpSpecies <- function(url=NULL, dataList = NULL, taxonomicInfo = FALSE, fullHigher = FALSE, getLink = FALSE, cores = max(1, parallel::detectCores() - 1))
{
  if(is.null(dataList))
  {
  species_list <- c()
  genus_list <- c()
  url_list <- c()
  
    if(is.null(url)){
      stop("\n No search url provided")
    }
    search <- rvest::read_html(url)
    ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")

    li_nodes <- xml2::xml_children(ul_element[[1]])
    for (i in seq_along(li_nodes)) {
      
      target <- xml2::xml_child(li_nodes[[i]], 1)
    
      species <- rvest::html_text(rvest::html_element(target, "em"), trim = TRUE)
      genus <- sub(" .*", "", species)
      href_raw <- xml2::xml_attrs(target)[["href"]]
      href <- sub("&search.*", "", href_raw)
      sppLink <- paste0("https://reptile-database.reptarium.cz",href)
      
      species_list <- c(species_list, species)
      genus_list <- c(genus_list, genus)
      url_list <- c(url_list, sppLink)
      
      percent <- (i/length(xml2::xml_children(ul_element[[1]]))) * 100
      cat(sprintf("\rGetting species links progress: %.1f%%", percent))
      utils::flush.console()
    }
    cat("\n")
    n_species <- length(species_list)
    
    if(taxonomicInfo == FALSE) {
      if(getLink == TRUE){
        searchResults <- data.frame(species = species_list,
                                    url = url_list,
                                    stringsAsFactors = FALSE)
  
        message_text <- paste0("A total of ", n_species, " species links retrieved.")
        
        }else{
        searchResults <- species_list
        
        message_text <- paste0("A total of ", n_species, " species retrieved.")
        }
      cat(" Data collection is done!\n", message_text, "\n")
      return(searchResults)
    }
  }else{
    species_list <- dataList$species
    genus_list <- sub(" .*", "", species_list)
    url_list <- dataList$url
    n_species <- length(species_list)
  }
  
# taxonomicInfo == TRUE ---------------------------------------------------
  if (taxonomicInfo == TRUE) {
    cat("Sampling species higher taxa progress:\n")
    
    orders <- c("Squamata", "Crocodylia", "Rhychocephalia", "Testudines")
    suborders <- c("Sauria", "Serpentes")
    
    results_list <- safeParallel(
      data = species_list,
      FUN = function(x) higherSample(
        x,
        species_list = species_list,
        genus_list = genus_list,
        url_list = url_list,
        orders = orders,
        suborders = suborders,
        fullHigher = fullHigher,
        getLink = getLink
      ),
      cores = cores
    )
    
    results_list <- Filter(Negate(is.null), results_list)
    searchResults <- as.data.frame(dplyr::bind_rows(results_list))
    return(searchResults)
    
  } # <--- closes if (taxonomicInfo == TRUE)
} # <--- closes herpSpecies function