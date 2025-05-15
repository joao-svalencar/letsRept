# HerpNom_sketch ----------------------------------------------------------
# populating species of interest list -------------------------------------
search <- rvest::read_html("https://reptile-database.reptarium.cz/advanced_search?genus=Apostolepis&submit=Search")
ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
ul_element


species_list <- c()

url_list <- c()

for(i in 1:length(xml2::xml_children(ul_element[[1]])))
{
  target <- xml2::xml_child(xml2::xml_child(ul_element[[1]], i), 1)
  
  species <- rvest::html_element(target, "em") |> rvest::html_text(trim = TRUE)
  url <- paste("https://reptile-database.reptarium.cz", xml2::xml_attrs(target)[["href"]], sep="")
  
  species_list <- c(species_list, species)
  url_list <- c(url_list, url)
}

uetz_br_scraped <- data.frame(family = family_list, species = species_list, url = url_list, stringsAsFactors = FALSE)

# scrapping synonyms ------------------------------------------------------

# BASICS ------------------------------------------------------------------
url <- rvest::read_html(uetz_br_scraped$url[1])
element <- rvest::html_element(url, "table") #scrap species table from Reptile Database
syn <- xml2::xml_child(element, 4) #select the synonym part of the table
td2 <- rvest::html_element(syn, "td:nth-child(2)")
children <- xml2::xml_contents(td2)
synonym_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(x) {
  if (length(x) >= 3 && x[2] %in% c("aff.", "cf", "gr.")) {
    paste(x[1:3], collapse = " ")
  } else {
    paste(x[1:2], collapse = " ")
  }
}))

data <- data.frame(species=c(rep(uetz_br_scraped$species[1], times=length(synonyms))), synonym=synonyms)

author <-sapply(strsplit(synonym_vector, " "), function(x) {
  if (length(x) > 2) paste(x[-(1:2)], collapse = " ")
  else NA})

# LOOP FOR ----------------------------------------------------------------
species_list <- c()
synonym_list <- c()


for(i in 1:length(uetz$species))
{
  url <- read_html(uetz$url[i])
  element <- html_element(url, "table") #scrap species table from Reptile Database
  
  #synonyms
  syn <- xml2::xml_child(element, 4) #select the synonym part of the table
  td_syn <- html_element(syn, "td:nth-child(2)")
  children <- xml_contents(td_syn)
  synonym_vector <- children[xml_name(children) == "text"] |> html_text(trim = TRUE)
  synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(y) {
    if (length(y) >= 3 && y[2] %in% c("aff.", "cf", "gr.","[sic]")|| grepl("^\\(.+\\)$", y[2])) {
      paste(y[1:3], collapse = " ")
    } else {
      paste(y[1:2], collapse = " ")
    }
  }))
  
  
  
  species <- c(rep(uetz$species[i], times=length(synonyms)))
  
  species_list <- c(species_list, species)
  synonym_list <- c(synonym_list, synonyms)
  print(uetz$species[i])
}

synonymResults <- data.frame(species = species_list, synonyms = synonym_list, stringsAsFactors = FALSE)


# getSpecies development --------------------------------------------------

getSpecies <- function(url)
{
  species_list <- c()
  family_list <- c()
  url_list <- c()
  
  search <- rvest::read_html(url)
  ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
  
  for(i in 1:length(xml2::xml_children(ul_element[[1]])))
  {
    target <- xml2::xml_child(xml2::xml_child(ul_element[[1]], i), 1)
    
    species <- rvest::html_element(target, "em") |> rvest::html_text(trim = TRUE)
    url <- paste("https://reptile-database.reptarium.cz", xml2::xml_attrs(target)[["href"]], sep="")
    
    species_list <- c(species_list, species)
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
  
  searchResults <- data.frame(family = family_list, species = species_list, url = url_list, stringsAsFactors = FALSE)
  return(searchResults)
}
