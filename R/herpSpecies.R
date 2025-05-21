##########################################################################################################
######################## function herpSpecies by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Reptile species summary
#'
#' @description 
#' Creates a data frame containing higher taxa information for a list of reptile species based on a Reptile Database advanced search optionally with their respective url.
#' 
#' @usage herpSpecies(url, 
#'                    higherTaxa=TRUE, 
#'                    fullHigher=FALSE, 
#'                    getLink=FALSE, 
#'                    batches=1, 
#'                    startBatch=1)
#' 
#' @param url A character string with the url from an advanced search in Reptile Database website or from letsHerp::herpAdvancedSearch.
#' @param higherTaxa A logical value indicating if user wants higher taxa information (specifically: Order, Suborder, Family and Genus) for each species. default = *TRUE*
#' @param fullHigher A logical value indicating if user wants the full higher taxa information (including e.g.: subfamily) for each species, as available in The Reptile Database website (e.g. single character string). default = *FALSE*. OBS.: Requires higherTaxa = TRUE
#' @param getLink A logical value indicating if user wants the url that provides access to each species information (e.g: to use with herpSynonyms()). default = *TRUE*
#' @param batches description
#' @param startBatch description
#'
#' @return if higherTaxa = FALSE (default), the function returns a vector with the list of species
#' 
#' if higherTaxa = TRUE, the function returns a data frame with columns: order, suborder (e.g.: Sauria or Serpentes only; when available), family, genus and species
#' 
#' Optionally, the function might return a data frame with a column with the full higher taxa information as reported in The Reptile Database, and the species respective url (necessary if looking for synonyms afterwards)
#' 
#' @examples
#' boaLink <- herpAdvancedSearch(genus = "Boa")
#' boa <- herpSpecies(boaLink, higherTaxa = TRUE)
#' boa <- herpSpecies(boaLink, getLink=TRUE, higherTaxa = FALSE)
#' 
#' @export
#'
herpSpecies <- function(url, higherTaxa = TRUE, fullHigher = FALSE, getLink = FALSE, batches = 1, startBatch = 1)
{
  species_list <- c()
  genus_list <- c()
  url_list <- c()
  taxa_vector_list <- c()
  order_list <- c()
  suborder_list <- c()
  family_list <- c()
  
  search <- rvest::read_html(url)
  ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
  
  for(i in 1:length(xml2::xml_children(ul_element[[1]])))
  {
    #add random sleep time
    Sys.sleep(stats::runif(1, min = 0.3, max = 1)) # random sleep time
    
    target <- xml2::xml_child(xml2::xml_child(ul_element[[1]], i), 1)
    
    species <- rvest::html_element(target, "em") |> rvest::html_text(trim = TRUE)
    genus <- sub(" .*", "", species)
    url <- paste("https://reptile-database.reptarium.cz", xml2::xml_attrs(target)[["href"]], sep="")
    
    species_list <- c(species_list, species)
    genus_list <- c(genus_list, genus)
    url_list <- c(url_list, url)
    
    percent <- (i/length(xml2::xml_children(ul_element[[1]]))) * 100
    cat(sprintf("\rGetting species links progress: %.1f%%", percent))
    utils::flush.console()
  }
  cat("\n")
    #code with batch:
  if (higherTaxa == TRUE) {
    
    match_taxon <- function(taxa_vector, rank_list) {
      match_idx <- which.max(vapply(rank_list, function(rank) any(stringr::str_detect(taxa_vector, rank)), logical(1)))
      result <- rank_list[match_idx]
      if (identical(result, character(0))) NA else result
    }
    
    orders <- c("Squamata", "Crocodylia", "Rhychocephalia", "Testudines")
    suborders <- c("Sauria", "Serpentes")
    
    # Default values if not provided
    if (is.null(batches)) batches <- 1
    if (is.null(startBatch)) startBatch <- 1
    
    total_species <- length(species_list)
    batch_size <- ceiling(total_species / batches)
    
    success <- TRUE
    
    for (b in startBatch:batches) {
      from <- ((b - 1) * batch_size) + 1
      to <- min(b * batch_size, total_species)
      
      message(sprintf("\nProcessing batch %d of %d: species %d to %d", b, batches, from, to))
      
      batch_success <- tryCatch({
        for (j in from:to) {
          sp_page <- rvest::read_html(url_list[j])
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
      
      if (!batch_success) break
    }
    
    # Final output — always return as much as was scraped
    n <- length(order_list)
    searchResults <- data.frame(
                      order = order_list,
                      suborder = suborder_list,
                      family = family_list,
                      genus = genus_list[1:n],
                      species = species_list[1:n],
                      stringsAsFactors = FALSE
                      )
    
    if (getLink == TRUE) {
      searchResults$url <- url_list[1:n]
    }
    
    return(searchResults)
  }
  
  #if(higherTaxa== TRUE){
    # # to get higher taxa information ------------------------------------------
    # match_taxon <- function(taxa_vector, rank_list) {
    #   match_idx <- which.max(vapply(rank_list, function(rank) any(stringr::str_detect(taxa_vector, rank)), logical(1)))
    #   result <- rank_list[match_idx]
    #   if (identical(result, character(0))) NA else result
    # }
    # 
    # orders <- c("Squamata", "Crocodylia", "Rhychocephalia", "Testudines") #order count = OK
    # suborders <- c("Sauria", "Serpentes") #suborder count = OK
    # 
    # if (is.null(num_batches)) num_batches <- 1
    # if (is.null(start_batch)) start_batch <- 1
    # 
    # total_species <- length(species_list)
    # batch_size <- ceiling(total_species / num_batches)
    
    # species_links <- tryCatch({
    #   for(j in 1:length(species_list))
    #   {
    #     sp_page <- rvest::read_html(url_list[j])
    #     element <- rvest::html_element(sp_page, "table") #scrap species table from Reptile Database
    #     taxa <- xml2::xml_child(element, 1) #select the higher taxa part of the table
    #     td_taxa <- rvest::html_element(taxa, "td:nth-child(2)")
    #     children <- xml2::xml_contents(td_taxa)
    #     
    #     #each species Higher Taxa information:
    #     taxa_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
    #     family <- stringr::str_extract(taxa_vector, "\\b[A-Z][a-z]+idae\\b")
    #     order <- match_taxon(taxa_vector, orders)
    #     #order <- orders[stringr::str_detect(taxa_vector, orders)][1] #get respective item from orders
    #     suborder <- match_taxon(taxa_vector, suborders)
    #     #suborder <- suborders[stringr::str_detect(taxa_vector, suborders)][1] #get respective item from suborders
    #     
    #     taxa_vector_list <- c(taxa_vector_list, taxa_vector)
    #     order_list <- c(order_list, order)
    #     suborder_list <- c(suborder_list, suborder)
    #     family_list <- c(family_list, family)
    #     
    #     cat("\n")
    #     percent <- (j/length(species_list)) * 100
    #     cat(sprintf("Getting higher taxa progress: %.1f%%", percent))
    #     flush.console()
    #   }
    #   
    # }, error = function(e){
    #   
    #   if(getLink==TRUE){
    #     message("\n Higher taxa scraping failed. Returning only species and links.")
    #     data.frame(
    #       species = species_list,
    #       genus = genus_list,
    #       url = url_list,
    #       stringsAsFactors = FALSE
    #     )
    #   }else{
    #     message("\n Higher taxa scraping failed. Returning only species list.")
    #     data.frame(
    #       species = species_list,
    #       genus = genus_list,
    #       stringsAsFactors = FALSE
    #     )
    #   }
    #   
    # })# closes tryCatch
    # return(species_links)
    # }
  #}#closes higher taxa = TRUE
  
# getLink == FALSE --------------------------------------------------------
   if(higherTaxa==FALSE && getLink==FALSE)
   { 
      searchResults <- species_list
      cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species retrieved."))
      return(searchResults)
      
   }else if (higherTaxa==TRUE && getLink==FALSE)
     {
       n <- length(order_list)
       searchResults <- data.frame(order = order_list,
                                   suborder = suborder_list,
                                   family = family_list,
                                   genus = genus_list[1:n],
                                   species = species_list[1:n],
                                   stringsAsFactors = FALSE)
       if(fullHigher==TRUE)
       {
         searchResults$higher_taxa <- taxa_vector_list
       }
       cat(" Data collection is done!", "\n", paste0("A total of ", length(species_list), " species higher taxa information retrieved."))
       return(searchResults)
     }

# getLink == TRUE ---------------------------------------------------------
     if(higherTaxa ==FALSE && getLink==TRUE)
     {
       searchResults <- data.frame(species = species_list,
                                   url = url_list)
       cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species links retrieved."))
       return(searchResults)
     }else if (higherTaxa==TRUE && getLink==TRUE)
       {
       n <- length(order_list)
       searchResults <- data.frame(order = order_list,
                                   suborder = suborder_list,
                                   family = family_list,
                                   genus = genus_list[1:n],
                                   species = species_list[1:n],
                                   url = url_list[1:n],
                                   stringsAsFactors = FALSE)
         if(fullHigher==TRUE)
         {
          searchResults$higher_taxa <- taxa_vector_list
         }
       cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species links and higher taxa information retrieved."))
       return(searchResults)
     }
}
