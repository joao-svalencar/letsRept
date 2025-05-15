library(rvest)
library(xml2)
library(stringr)

uetz_br <- read.table(here::here("data", "nomenclature", "reptile_database.txt"), header = TRUE, sep="\t")

# BASICS ------------------------------------------------------------------
url <- read_html(uetz_br_scraped$url[1])
element <- html_element(url, "table") #scrap species table from Reptile Database
syn <- xml2::xml_child(element, 4) #select the synonym part of the table
td2 <- html_element(syn, "td:nth-child(2)")
children <- xml_contents(td2)
synonym_vector <- children[xml_name(children) == "text"] |> html_text(trim = TRUE)
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
for(i in 2:20)
{
  url <- read_html(uetz_br_scraped$url[i])
  element <- html_element(url, "table") #scrap species table from Reptile Database
  syn <- xml2::xml_child(element, 4) #select the synonym part of the table
  td2 <- html_element(syn, "td:nth-child(2)")
  children <- xml_contents(td2)
  synonym_vector <- children[xml_name(children) == "text"] |> html_text(trim = TRUE)
  synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(x) {
    if (length(x) >= 3 && x[2] %in% c("aff.", "cf", "gr.","[sic]")) {
      paste(x[1:3], collapse = " ")
    } else {
      paste(x[1:2], collapse = " ")
    }
  }))
  
  data2 <- data.frame(species = c(rep(uetz_br_scraped$species[i], times=length(synonyms))),
                      synonym = synonyms)
  
  data <- rbind(data, data2)
  print(uetz_br_scraped$species[i])
}

write.csv(data, here::here("data", "nomenclature", "snakes_synonym.csv"),fileEncoding = "utf-8", row.names = FALSE)


# Scrapping species list and url ------------------------------------------

page <- read_html("https://reptile-database.reptarium.cz/advanced_search?taxon=snake&location=Brazil&submit=Search")

ul_element <- html_elements(page, "#content > ul:nth-child(6)")
ul_element

species_list <- c()
url_list <- c()

for(i in 1:length(xml_children(ul_element[[1]])))
{
  target <- xml_child(xml_child(ul_element[[1]], i), 1)
  
  species <- html_element(target, "em") |> html_text(trim = TRUE)
  url <- paste("https://reptile-database.reptarium.cz", xml_attrs(target)[["href"]], sep="")
  
  species_list <- c(species_list, species)
  url_list <- c(url_list, url)
}

uetz_br_scraped <- data.frame(species = species_list, url = url_list, stringsAsFactors = FALSE)

unique(uetz_br$species)[which(!unique(uetz_br$species)%in%unique(uetz_br_scraped$species))]
