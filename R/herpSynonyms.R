##########################################################################################################
####################### function herpSynonyms by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Get reptile species synonyms
#'
#' @description
#' creates a _dataframe_ containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' @param x a _dataframe_ columns: 'species' and 'url' (their respective Reptile Database url). Could be the output of letsHerp::herpSpecies()
#'
#' @returns '_herpSynonyms_' returns a dataframe with columns: species and their respective synonyms according to the version of Reptile Database.
#' 
#' @references 
#' Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database, http://www.reptile-database.org
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @export
#'

herpSynonyms <- function(x)
{
  species_list <- c()
  synonym_list <- c()
  synonym_vector_list <- c()
  
  for(i in 1:length(x$species))
  {
    #add random sleep time
    Sys.sleep(stats::runif(1, min = 0.3, max = 1)) # random sleep time
    
    url <- rvest::read_html(x$url[i])
    element <- rvest::html_element(url, "table") #scrap species table from Reptile Database
    
    #synonyms
    syn <- xml2::xml_child(element, 4) #select the synonym part of the table
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    synonym_vector <- unique(children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE))
    
    synonyms <- sub(
        "^\\W*\\s*([A-Z][a-z]+(?:\\s+[a-z]+){1,3}(?:\\s+\\[sic\\])?)\\b.*",
        "\\1",
        iconv(synonym_vector, to = "ASCII//TRANSLIT"),
        perl = TRUE
      )
    
    cat(paste("Species number",paste0(i,"",":"), "\n", x$species[i],"\n", "Done!", "\n", "\n"))
    species <- c(rep(x$species[i], times=length(synonyms)))
    
    species_list <- c(species_list, species)
    synonym_list <- c(synonym_list, synonyms)
  
}   #loop for ends here
    synonymResults <- data.frame(species = species_list,
                                 synonyms = synonym_list,
                                 stringsAsFactors = FALSE)
  #
  return(synonymResults)
}
