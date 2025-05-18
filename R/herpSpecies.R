##########################################################################################################
######################## function herpSpecies by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Building url species list 
#'
#' @description 
#' Creates a _dataframe_ containing a list of species based on a Reptile Database advanced search with their respective url
#' 
#' @usage herpSpecies(url, higherTaxa = TRUE, fullHigher = FALSE, getlink = FALSE)
#' 
#' @param url A _character_ string with the url from an advanced search in Reptile Database.
#' @param higherTaxa A _logical_ value indicating if user wants higher taxa information (specifically: Order, Suborder, Family and Genus) for each species retrieved. default = *TRUE*.
#' @param fullHigher A _logical_ value indicating if user wants the full higher taxa information (including e.g.: subfamily) for each species retrieved, as available in The Reptile Database website (e.g. single character string). default = *FALSE*. OBS.: Requires higherTaxa = TRUE.
#' @param getlink A _logical_ value indicating if user wants the url that provides access to each retrieved species (e.g: to use with herpSynonyms()). default = *TRUE*
#'
#' @return if _higherTaxa_ = FALSE (default), '_herpSpecies_' returns a dataframe with columns: species and url.
#' 
#' if _higherTaxa_ = TRUE, '_herpSpecies_' returns a dataframe with columns: family, genus, species and url
#' 
#' @references 
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @export
#'
herpSpecies <- function(url, higherTaxa = TRUE, fullHigher = FALSE, getlink = FALSE)
{
  species_list <- c()
  genus_list <- c()
  url_list <- c()
  
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
  }

# to get higher taxa information ------------------------------------------
     taxa_vector_list <- c()
     order_list <- c()
     suborder_list <- c()
     family_list <- c()
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
      family <- sub(" .*", "", taxa_vector) #always the first item in Higher Taxa
      order <- orders[stringr::str_detect(taxa_vector, orders)][1] #get respective item from orders
      suborder <- suborders[stringr::str_detect(taxa_vector, suborders)][1] #get respective item from suborders
      
      taxa_vector_list <- c(taxa_vector_list, taxa_vector)
      order_list <- c(order_list, order)
      suborder_list <- c(suborder_list, suborder)
      family_list <- c(family_list, family)
     }

# getlink == FALSE --------------------------------------------------------
   if(getlink==FALSE)
   { 
     if(higherTaxa ==FALSE)
     {
      searchResults <- species_list
      cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species retrieved."))
      return(searchResults)
     }else{ 
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
     cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species higher taxa information retrieved."))
     return(searchResults)
     }
     
   }else
   { 

# getlink == TRUE ---------------------------------------------------------
     if(higherTaxa ==FALSE)
     {
       searchResults <- data.frame(species = species_list,
                                   url = url_list)
       cat(" Data collection is done!","\n", paste0("A total of ", length(species_list), " species links retrieved."))
       return(searchResults)
     }else{ 
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
      
}