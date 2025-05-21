##########################################################################################################
######################## function herpSpecies by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Reptile species summary 
#'
#' @description 
#' Creates a data frame containing higher taxa information for a list of reptile species based on a Reptile Database advanced search optionally with their respective url.
#' 
#' @usage herpSpecies(url, higherTaxa = TRUE, fullHigher = FALSE, getLink = FALSE)
#' 
#' @param url A character string with the url from an advanced search in Reptile Database website or from letsHerp::herpAdvancedSearch.
#' @param higherTaxa A logical value indicating if user wants higher taxa information (specifically: Order, Suborder, Family and Genus) for each species. default = *TRUE*
#' @param fullHigher A logical value indicating if user wants the full higher taxa information (including e.g.: subfamily) for each species, as available in The Reptile Database website (e.g. single character string). default = *FALSE*. OBS.: Requires higherTaxa = TRUE
#' @param getLink A logical value indicating if user wants the url that provides access to each species information (e.g: to use with herpSynonyms()). default = *TRUE*
#'
#' @return if higherTaxa = FALSE (default), the function returns a vector with the list of species
#' 
#' if _igherTaxa = TRUE, the function returns a data frame with columns: order, family, genus and species
#' 
#' Optionally, the function might return a data frame with a column with the full higher taxa information as reported in The Reptile Database, and the species respective url
#' 
#' @references 
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @export
#'
herpSpecies <- function(url, higherTaxa = TRUE, fullHigher = FALSE, getLink = FALSE)
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
    
    if(higherTaxa==FALSE){
      percent <- (i/length(xml2::xml_children(ul_element[[1]]))) * 100
      cat(sprintf("\rProgress: %.1f%%", percent))
      flush.console()
    }
    
  }

  if(higherTaxa== TRUE){
    # to get higher taxa information ------------------------------------------
    orders <- c("Squamata", "Crocodylia", "Rhychocephalia", "Testudines") #order count = OK
    suborders <- c("Sauria", "Serpentes") #suborder count = OK
    for(j in 1:length(species_list))
    {
      sp_page <- rvest::read_html(url_list[j])
      element <- rvest::html_element(sp_page, "table") #scrap species table from Reptile Database
      taxa <- xml2::xml_child(element, 1) #select the higher taxa part of the table
      td_taxa <- rvest::html_element(taxa, "td:nth-child(2)")
      children <- xml2::xml_contents(td_taxa)
      
      #each species Higher Taxa information:
      taxa_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
      family <- sub("^([A-Z][a-zA-Z]*)\\b.*", "\\1", taxa_vector)
      order <- orders[stringr::str_detect(taxa_vector, orders)][1] #get respective item from orders
      suborder <- suborders[stringr::str_detect(taxa_vector, suborders)][1] #get respective item from suborders
      
      taxa_vector_list <- c(taxa_vector_list, taxa_vector)
      order_list <- c(order_list, order)
      suborder_list <- c(suborder_list, suborder)
      family_list <- c(family_list, family)
      
      percent <- (j/length(species_list)) * 100
      cat(sprintf("\rProgress: %.1f%%", percent))
      flush.console()
    }
  }
  
# getLink == FALSE --------------------------------------------------------
   if(higherTaxa==FALSE && getLink==FALSE)
   { 
      searchResults <- species_list
      cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species retrieved."))
      return(searchResults)
      
   }else if (higherTaxa==TRUE && getLink==FALSE)
     {
       searchResults <- data.frame(order = order_list,
                                   suborder = suborder_list,
                                   family = family_list,
                                   genus = genus_list,
                                   species = species_list,
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
       searchResults <- data.frame(order = order_list,
                                   suborder = suborder_list,
                                   family = family_list,
                                   genus = genus_list,
                                   species = species_list,
                                   url = url_list,
                                   stringsAsFactors = FALSE)
         if(fullHigher==TRUE)
         {
          searchResults$higher_taxa <- taxa_vector_list
         }
       cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species links and higher taxa information retrieved."))
       return(searchResults)
     }
}
