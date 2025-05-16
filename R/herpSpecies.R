##########################################################################################################
######################## function herpSpecies by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Building url species list 
#'
#' @description 
#' creates a _dataframe_ containing list of species based on a Reptile Database advanced search with their respective url
#' @usage herpSpecies(url, higherTaxa = FALSE)
#' @param url a _character_ string with the url from an advanced search in Reptile Database
#' @param higherTaxa a _logical_ value indicating if user wants the families for each species within the resulting data frame
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

herpSpecies <- function(url, higherTaxa = FALSE)
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
   if(higherTaxa==TRUE)
   {
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
      family <- sub(" .*", "", taxa_vector)
      order <- orders[stringr::str_detect(taxa_vector, orders)][1]
      suborder <- suborders[stringr::str_detect(taxa_vector, suborders)][1]
      
      taxa_vector_list <- c(taxa_vector_list, taxa_vector)
      order_list <- c(order_list, order)
      suborder_list <- c(suborder_list, suborder)
      family_list <- c(family_list, family)
     }
      searchResults <- data.frame(higher_taxa = taxa_vector_list,
                                  order = order_list,
                                  suborder = suborder_list,
                                  family = family_list,
                                  genus = genus_list,
                                  species = species_list,
                                  url = url_list,
                                  stringsAsFactors = FALSE)
   }else{
  searchResults <- data.frame(species = species_list,
                              url = url_list,
                              stringsAsFactors = FALSE)
  }
  
  return(searchResults)
}
