library(rvest)
library(xml2)
library(stringr)

# Scrapping species list and url ------------------------------------------

getSpecies <- function(url)
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
  
  #family
  for(j in 1:length(species_list))
  {
    sp_page <- read_html(url_list[j])
    element <- html_element(sp_page, "table") #scrap species table from Reptile Database
    taxa <- xml2::xml_child(element, 1) #select the higher taxa part of the table
    td_taxa <- html_element(taxa, "td:nth-child(2)")
    children <- xml_contents(td_taxa)
    taxa_vector <- children[xml_name(children) == "text"] |> html_text(trim = TRUE)
    family_vector <- sub(",.*", "", taxa_vector)
    
    family_list <- c(family_list, family_vector)
  }
  
  searchResults <- data.frame(family = family_list, genus = genus_list, species = species_list, url = url_list, stringsAsFactors = FALSE)
  return(searchResults)
}


uetz <- getSpecies("https://reptile-database.reptarium.cz/advanced_search?genus=%22Boa%22&location=-starwars&submit=Search")

getSynonyms <- function(x, getAuthor = FALSE)
{
  species_list <- c()
  synonym_list <- c()
  
  for(i in 1:length(x$species))
  {
    url <- rvest::read_html(x$url[i])
    element <- rvest::html_element(url, "table") #scrap species table from Reptile Database
    
    #synonyms
    syn <- xml2::xml_child(element, 4) #select the synonym part of the table
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    synonym_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
    synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(y) {
      if (length(y) >= 3 && y[2] %in% c("aff.", "cf", "gr.","[sic]") || grepl("^\\(.+\\)$", y[2])) {
        paste(y[1:3], collapse = " ")
      } else {
        paste(y[1:2], collapse = " ")
      }
    }))
    
    print(x$species[i])
    species <- c(rep(x$species[i], times=length(synonyms)))
    
    species_list <- c(species_list, species)
    synonym_list <- c(synonym_list, synonyms)

  }
  
  synonymResults <- data.frame(species = species_list, synonyms = synonym_list, stringsAsFactors = FALSE)
  return(synonymResults)
}

data <- getSynonyms(uetz)
