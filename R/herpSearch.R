#' Search The Reptile Database website (TRD): Simple
#' 
#' @description
#' Searches The Reptile Database website and provides the data of a single species
#' 
#' @usage herpSearch(binomial=NULL)
#' 
#' @param binomial A character string with the current valid binomial name of a given reptile species.
#'
#' @returns
#' Returns species information as provided by The Reptile Database.
#' 
#' @examples
#' herpSearch("Boa constrictor")
#' 
#' @export
#' 

herpSearch <- function(binomial=NULL){

if(!is.null(binomial))
{
  base_url <- "https://reptile-database.reptarium.cz/species"
  gen <- strsplit(binomial, " ")[[1]][1]
  species <- strsplit(binomial, " ")[[1]][2]
  query <- paste0("?genus=", gen, "&species=", species)
  sppLink <- paste0(base_url, query) #url for direct species search
  
  url <- rvest::read_html(sppLink)
  element <- rvest::html_element(url, "table") #get the content table for each species
  
  if(is.na(element))
  {
  cat(" Species not found.", "\n",
        "Check the spelling or try an advanced search for synonyms.")
    return(invisible(NULL))
  }
  
#Printing species information from The Reptile Database
      for(i in 1:10)
      {
        row <- xml2::xml_child(element, i)
        cells <- xml2::xml_children(row)
        
        if(length(cells) >= 2)
        {
          title <- rvest::html_text(cells[1], trim = TRUE)
          content_raw <- rvest::html_text(cells[2], trim = TRUE)
          
          if(content_raw == "")next
          
          if(i == 2)
          {
            br_items <- xml2::xml_add_sibling(
              rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
            
            syn_text <- rvest::html_text(cells[2], trim = TRUE)
            syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
            syn_list <- trimws(syn_list[syn_list != ""])
            
            cat("Common Names:\n")
            for (item in syn_list) {
              cat(" -", item, "\n")
            }
            cat("\n")
          }else
            if(i == 3)
          {
            br_items <- xml2::xml_add_sibling(
              rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
            
            syn_text <- rvest::html_text(cells[2], trim = TRUE)
            syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
            syn_list <- trimws(syn_list[syn_list != ""])
            
            cat("Common Names:\n")
            for (item in syn_list) {
              cat(" -", item, "\n")
            }
            cat("\n")
          }else
            if(i == 4)
          {
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
          }else 
            if(i == 5)
          {
            br_items <- xml2::xml_add_sibling(
              rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
            
            syn_text <- rvest::html_text(cells[2], trim = TRUE)
            syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
            syn_list <- trimws(syn_list[syn_list != ""])
            
            cat("Distribution:\n")
            for (item in syn_list) {
              cat(" -", item, "\n")
            }
            cat("\n")
          }else
            if(i == 7)
          {
            br_items <- xml2::xml_add_sibling(
              rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
            
            syn_text <- rvest::html_text(cells[2], trim = TRUE)
            syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
            syn_list <- trimws(syn_list[syn_list != ""])
            
            cat("Types:\n")
            for (item in syn_list) {
              cat(" -", item, "\n")
            }
            cat("\n")
          }else
            if(i == 9)
          {
            br_items <- xml2::xml_add_sibling(
              rvest::html_nodes(cells[[2]], "br"), "marker","<SPLIT>")
            
            syn_text <- rvest::html_text(cells[2], trim = TRUE)
            syn_list <- unlist(strsplit(syn_text, "<SPLIT>"))
            syn_list <- trimws(syn_list[syn_list != ""])
            
            cat("Comment:\n")
            for (item in syn_list) {
              cat(" -", item, "\n")
            }
            cat("\n")
          }else{
            content <- rvest::html_text(cells[2], trim = TRUE)
            cat(paste0(title,":"),"\n")
            cat(content,"\n\n")
          }
        }
      }
  }else
  {
  cat(" Species current valid binomial not provided.")
  }
  return(invisible(NULL))
}