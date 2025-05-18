#' Search The Reptile Database website (TRD): Simple
#' 
#' @description
#' Simulates searches in The Reptile Database website and provides the data of a single species or the url for multiple species sampling by herpSpecies()\\cr
#' #' *ATTENTION:* under development, may not work yet.\\cr
#' Alternatively, for advanced search copy the link with the results from TRD and run herpSpecies()
#' 
#' @usage herpSearch(binomial=NULL)
#' 
#' @param binomial a _character_ string with the current valid binomial name of a given reptile species (e.g.: "_Apostolepis adhara_")
#'
#' @returns
#' Returns species information collected from its respective page in The Reptile Database.
#' 
#' @export
#'

herpSearch <- function(binomial=NULL){

# block for single species search -----------------------------------------
#for binomial:
if(!is.null(binomial))
{
  base_url <- "https://reptile-database.reptarium.cz/species"
  gen <- strsplit(binomial, " ")[[1]][1]
  species <- strsplit(binomial, " ")[[1]][2]
  query <- paste0("?genus=", gen, "&species=", species)
  url <- paste0(base_url, query) #url for direct species search
  
  element <- rvest::html_element(url, "table") #get the content table for each species
  
  #pasting the content of Higher Taxa, Subspecies, Common Names, Synonyms, Distribution and Reproduction when available:
  for (i in 1:6)
  {
    row <- xml2::xml_child(element, i)
    cells <- xml2::xml_children(row)
    
    if (length(cells) >= 2) {
      title <- rvest::html_text(cells[1], trim = TRUE)
      content_raw <- rvest::html_text(cells[2], trim = TRUE)
      
      if (content_raw == "") next
      
      if (i == 4) {
        # For Synonym row: split by <br> and trim
        # br_items <- cells[2] %>%
        #   rvest::html_nodes("br") %>%
        #   xml2::xml_add_sibling("marker", "<SPLIT>") # helper tag to split text
        
        br_items <- xml2::xml_add_sibling(
          rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
        
        syn_text <- rvest::html_text(cells[2], trim = TRUE)
        syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
        syn_list <- trimws(syn_list[syn_list != ""])
        
        cat("Synonyms:\n")
        for (item in syn_list) {
          cat(" -", item, "\n")
        }
        cat("\n")
      } else {
        content <- rvest::html_text(cells[2], trim = TRUE)
        cat(paste0(title,":"),"\n")
        cat(content,"\n\n")
      }
    }
  } #closes the species content printing
}else{
  cat(" Species not found.", "\n",
      "Check the spelling or try an advanced search for synonyms.")
}
  return(invisible(NULL))
}
  