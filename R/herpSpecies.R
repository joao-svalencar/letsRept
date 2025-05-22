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
#'                    taxonomicInfo=TRUE, 
#'                    fullHigher=FALSE, 
#'                    getLink=FALSE, 
#'                    batches=1, 
#'                    startBatch=1)
#'                    
#' @param url A character string with the url from an advanced search in Reptile Database website or from letsHerp::herpAdvancedSearch.
#' @param dataList A data frame with columns: species and url, only for sampling taxonomicInfo from already sampled species links.
#' @param taxonomicInfo A logical value indicating if user wants full species taxonomic information (specifically: Order, Suborder, Family, Genus, species author and description year) for each species. default = *TRUE*
#' @param fullHigher A logical value indicating if user wants the full higher taxa information (including e.g.: subfamily) for each species, as available in The Reptile Database website (e.g. single character string). default = *FALSE*. OBS.: Requires taxonomicInfo = TRUE
#' @param getLink A logical value indicating if user wants the url that provides access to each species information (e.g: to use with herpSynonyms()). default = *TRUE*
#' @param batches An integer indicating in how many batches user would like to divide data collection (good for large datasets). 
#' @param startBatch An integer indicating where to start data collection (e.g.: if failed in previous attempt).
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
herpSpecies <- function(url=NULL, dataList = NULL, taxonomicInfo = TRUE, fullHigher = FALSE, getLink = FALSE, batches = 1, startBatch = 1)
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
  
  for(i in 1:length(xml2::xml_children(ul_element[[1]])))
  {
    #add random sleep time
    Sys.sleep(stats::runif(1, min = 0.3, max = 1)) # random sleep time
    
    li_node <- xml2::xml_child(ul_element[[1]], i)
    target <- xml2::xml_child(li_node, 1)
    
    species <- rvest::html_element(target, "em") |> rvest::html_text(trim = TRUE)
    genus <- sub(" .*", "", species)
    sppLink <- paste("https://reptile-database.reptarium.cz", xml2::xml_attrs(target)[["href"]], sep="")
    
    species_list <- c(species_list, species)
    genus_list <- c(genus_list, genus)
    url_list <- c(url_list, sppLink)
    
    percent <- (i/length(xml2::xml_children(ul_element[[1]]))) * 100
    cat(sprintf("\rGetting species links progress: %.1f%%", percent))
    utils::flush.console()
  }
  cat("\n")
  
  if(taxonomicInfo == FALSE) {
    if(getLink == TRUE){
      searchResults <- data.frame(species = species_list,
                                  url = url_list,
                                  stringsAsFactors = FALSE)

      message_text <- paste0("A total of ", length(species_list), " species links retrieved.")
      
    }else{
      searchResults <- species_list
      
      message_text <- paste0("A total of ", length(species_list), " species retrieved.")
    }
    cat(" Data collection is done!\n", message_text, "\n")
    return(searchResults)
    }
  }else{
    species_list <- dataList$species
    genus_list <- sub(" .*", "", species_list)
    url_list <- dataList$url
# taxonomicInfo == TRUE ---------------------------------------------------
  #code with batch:
  if (taxonomicInfo == TRUE) {
    
    taxa_vector_list <- c()
    order_list <- c()
    suborder_list <- c()
    family_list <- c()
    sppAuthor_list <- c()
    sppYear_list <- c()
    
    orders <- c("Squamata", "Crocodylia", "Rhychocephalia", "Testudines")
    suborders <- c("Sauria", "Serpentes")

    match_taxon <- function(taxa_vector, rank_list) {
      matches <- rank_list[sapply(rank_list, function(rank) stringr::str_detect(taxa_vector, rank))]
      if(length(matches)==0){
        return(NA)
      }
      match_positions <- sapply(matches, function(rank) stringr::str_locate(taxa_vector, rank)[1])
      sorted_matches <- matches[order(match_positions)]
      return(paste(sorted_matches, collapse = ", "))
      }
    
    # Default values if not provided
    if (is.null(batches)) batches <- 1
    if (is.null(startBatch)) startBatch <- 1
    
    total_species <- length(species_list)
    batch_size <- ceiling(total_species / batches)
    
    success <- TRUE
    
    for(b in startBatch:batches){
      from <- ((b - 1) * batch_size) + 1
      to <- min(b * batch_size, total_species)
      
      message(sprintf("\nProcessing batch %d of %d: species %d to %d", b, batches, from, to))
      
      batch_success <- tryCatch({
        for (j in from:to) {
          sp_page <- rvest::read_html(url_list[j])
          title <- rvest::html_element(sp_page, "h1")
          
          sppAuthor <- rvest::html_text(title, trim = TRUE)
          sppAuthor <- gsub("^([A-Z][a-z]+\\s+[a-z\\-]+)\\s*", "", sppAuthor)
          sppAuthor <- gsub("\\s{2,}", " ", sppAuthor)
          sppAuthor <- trimws(gsub("\\s+", " ", sppAuthor))
          
          sppYear <- stringr::str_extract(sppAuthor, "\\d{4}")
          
          element <- rvest::html_element(sp_page, "table")
          taxa <- xml2::xml_child(element, 1)
          td_taxa <- rvest::html_element(taxa, "td:nth-child(2)")
          children <- xml2::xml_contents(td_taxa)
          
          taxa_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
          family <- stringr::str_extract(taxa_vector, "\\b[A-Z][a-z]+idae\\b")
          order <- match_taxon(taxa_vector, orders)
          suborder <- match_taxon(taxa_vector, suborders)
          
          taxa_vector_list <- c(taxa_vector_list, taxa_vector)
          order_list <- c(order_list, order)
          suborder_list <- c(suborder_list, suborder)
          family_list <- c(family_list, family)
          sppAuthor_list <- c(sppAuthor_list, sppAuthor)
          sppYear_list <- c(sppYear_list, sppYear)
          
          percent <- (j / total_species) * 100
          cat(sprintf("\rGetting higher taxa progress: %.1f%%", percent))
          flush.console()
        }
        TRUE  # batch_success
      }, error = function(e) {
        message(sprintf("\nError in batch %d. Returning all successfully completed batches (%d/%d).", b, b - 1, batches))
        success <<- FALSE
        return(FALSE)
      })
      if(!batch_success)break
    }
    
    # Final output — always return as much as was scraped
    n <- length(family_list)
    searchResults <-  data.frame(
                      order = order_list,
                      suborder = suborder_list,
                      family = family_list,
                      genus = genus_list[1:n],
                      species = species_list[1:n],
                      year = sppYear_list[1:n],
                      author = sppAuthor_list[1:n],
                      stringsAsFactors = FALSE
                      )
    
      if (getLink == TRUE) 
      {
        searchResults$url <- url_list[1:n]
      }
    }
  }
  return(searchResults)
}
  