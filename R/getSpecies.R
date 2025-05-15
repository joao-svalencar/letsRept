##########################################################################################################
######################## function getSpecies by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Building url species list 
#'
#' @description
#' @usage getSpecies(url, higherTaxa = FALSE)
#' @param url a _character_ string with the url from an advanced search in Reptile Database
#' @param higherTaxa a _logical_ value indicating if user wants the families for each species within the resulting data frame
#'
#' @return if _higherTaxa_ = FALSE (default), '_getSpecies_' returns a dataframe with columns: species and url.
#' 
#' if _higherTaxa_ = TRUE, '_getSpecies_' returns a dataframe with columns: family, genus, species and url
#' 
#' @references 
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @export
#'

getSpecies <- function(url, higherTaxa = FALSE)
{
  species_list <- c()
  genus_list <- c()
  family_list <- c()
  url_list <- c()
  
  search <- rvest::read_html(url)
  ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
  
  for(i in 1:length(xml2::xml_children(ul_element[[1]])))
  {
    target <- xml2::xml_child(xml2::xml_child(ul_element[[1]], i), 1)
    
    species <- rvest::html_element(target, "em") |> rvest::html_text(trim = TRUE)
    genus <- sub(" .*", "", species)
    url <- paste("https://reptile-database.reptarium.cz", xml2::xml_attrs(target)[["href"]], sep="")
    
    species_list <- c(species_list, species)
    genus_list <- c(genus_list, genus)
    url_list <- c(url_list, url)
  }
  
  #higherTaxa
  if(higherTaxa==TRUE)
  {
    for(j in 1:length(species_list))
    {
      sp_page <- rvest::read_html(url_list[j])
      element <- rvest::html_element(sp_page, "table") #scrap species table from Reptile Database
      taxa <- xml2::xml_child(element, 1) #select the higher taxa part of the table
      td_taxa <- rvest::html_element(taxa, "td:nth-child(2)")
      children <- xml2::xml_contents(td_taxa)
      taxa_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
      family_vector <- sub(" .*", "", taxa_vector)
      
      family_list <- c(family_list, family_vector)
    }
    searchResults <- data.frame(family = family_list, genus = genus_list, species = species_list, url = url_list, stringsAsFactors = FALSE)
  }else{
    searchResults <- data.frame(species = species_list, url = url_list, stringsAsFactors = FALSE)
  }
  
  return(searchResults)
}
