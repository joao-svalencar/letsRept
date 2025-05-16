##########################################################################################################
####################### function getSynonyms by:  JP VIEIRA-ALENCAR  #####################################
##########################################################################################################

#' Get reptile species synonyms
#'
#' @description
#' creates a _dataframe_ containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' @param x a _dataframe_ with columns: 'species' and 'url' (their respective Reptile Database url). Could be the output of HerpNom::getSpecies()
#' @param getRef a _logical_ value. default is FALSE. TRUE returns an extra column with the abbreviated reference that used each synonym
#'
#' @returns if _getRef_ = FALSE (default) '_getSynonyms_' returns a dataframe with columns: species and their respective synonyms according to the version of Reptile Database.
#' 
#' if _getRef_ = TRUE '_getSynonyms_' returns an additional column with the synonym and the abbreviated reference that used that nomenclature
#' 
#' @references 
#' Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database, http://www.reptile-database.org
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @export
#'

getSynonyms <- function(x, getRef = FALSE)
{
  species_list <- c()
  synonym_list <- c()
  
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
    synonym_vector <- children[xml2::xml_name(children) == "text"] |> rvest::html_text(trim = TRUE)
    synonyms <- unique(sapply(strsplit(synonym_vector, " "), function(y) {
      if (length(y) >= 4 && y[1] == "?" && y[3] %in% c("aff", "cf", "gr", "aff.", "cf.", "gr.","[sic]")) {
        paste(y[1:4], collapse = " ")
      } else if (length(y) >= 3 && y[1] == "?") {
        paste(y[1:3], collapse = " ")
      } else if (length(y) >= 3 && (y[2] %in% c("aff", "cf", "gr", "aff.", "cf.", "gr.","[sic]") || grepl("^\\(.+\\)$", y[2]))) {
        paste(y[1:3], collapse = " ")
      } else {
        paste(y[1:2], collapse = " ")
      }
    }))
    
    cat(paste("Species number",paste0(i,"",":"), "\n", x$species[i],"\n", "Done!", "\n", "\n"))
    species <- c(rep(x$species[i], times=length(synonyms)))
    
    species_list <- c(species_list, species)
    synonym_list <- c(synonym_list, synonyms)
    
  }
  
  synonymResults <- data.frame(species = species_list, synonyms = synonym_list, stringsAsFactors = FALSE)
  return(synonymResults)
}
