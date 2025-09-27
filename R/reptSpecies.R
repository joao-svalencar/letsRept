#' Retrieve Reptile Species and Taxonomic Information from RDB
#'
#' @description 
#' Retrieves a list of reptile species from The Reptile Database (RDB) based on a search URL, and optionally returns detailed taxonomic information for each species. 
#' This function can also save progress to disk during sampling and extract species-specific URLs for further use.
#'                    
#' @param url Character string. A search URL generated via an advanced search on the RDB website or with \code{\link{reptAdvancedSearch}}.
#' @param showProgress Logical. If \code{TRUE}, prints sampling progress in the console. Default is \code{FALSE}.
#' @param dataList Optional. A data frame with columns \code{species} and \code{url}, used to extract taxonomic information from previously sampled species links.
#' @param taxonomicInfo Logical. If \code{TRUE}, returns taxonomic information for each species, including order, suborder, family, genus, author, and year. Default is \code{FALSE}.
#' @param fullHigher Logical. If \code{TRUE}, includes the full higher taxonomic hierarchy as reported by RDB (e.g., including subfamilies). Requires \code{taxonomicInfo = TRUE}. Default is \code{FALSE}.
#' @param getLink Logical. If \code{TRUE}, includes the RDB URL for each species (useful for follow-up functions like \code{\link{reptSynonyms}}). Default is \code{FALSE}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is \code{cores = 1}.
#' 
#' @return
#' If \code{taxonomicInfo = FALSE} (default), returns a character vector of species names.  
#'  
#' If \code{taxonomicInfo = TRUE}, returns a data frame with columns:
#' \code{order}, \code{suborder} (if available), \code{family}, \code{genus}, \code{species}, \code{author}, and \code{year}.
#'  
#' If \code{fullHigher = TRUE}, includes an additional column with the full higher taxa classification.  
#'  
#' If \code{getLink = TRUE}, includes a column with the URL for each species’ page on RDB.
#'
#' @examples
#' \donttest{
#' boa <- reptSpecies(reptAdvancedSearch(genus = "Boa"),
#'                                       taxonomicInfo = TRUE, 
#'                                       cores = 2)
#' }
#' 
#' @seealso \code{\link{reptAdvancedSearch}}, \code{\link{reptSynonyms}}, \code{\link{reptSearch}}
#' 
#' @export
#'

reptSpecies <- function(url=NULL,
                        showProgress=TRUE,
                        dataList = NULL,
                        taxonomicInfo = FALSE,
                        fullHigher = FALSE,
                        getLink = FALSE,
                        cores = 1
                        )
{
  if(is.null(dataList))
  {
    species_list <- c()
    genus_list <- c()
    url_list <- c()
    
    if(is.null(url)){
      stop("\n No search url provided")
    }
    
    search <- safeRequest(url)
    ul_element <- rvest::html_elements(search, "#content > ul:nth-child(6)")
    
    li_nodes <- xml2::xml_children(ul_element[[1]])
    for (i in seq_along(li_nodes)) {
      
      target <- xml2::xml_child(li_nodes[[i]], 1)
      
      species <- rvest::html_text(rvest::html_element(target, "em"), trim = TRUE)
      genus <- sub(" .*", "", species)
      href_raw <- xml2::xml_attrs(target)[["href"]]
      href <- sub("&search.*", "", href_raw)
      sppLink <- paste0("https://reptile-database.reptarium.cz",href)
      
      species_list <- c(species_list, species)
      genus_list <- c(genus_list, genus)
      url_list <- c(url_list, sppLink)
      
      if(showProgress == TRUE){
      percent <- (i/length(xml2::xml_children(ul_element[[1]]))) * 100
        if(getLink){
          cat(sprintf("\rGetting species names and links progress: %.1f%%", percent))  
        }else{
          cat(sprintf("\rGetting species names progress: %.1f%%", percent))
        }
      utils::flush.console()
      }
    }
    
    n_species <- length(species_list)
    
    if(taxonomicInfo == FALSE) {
      if(getLink == TRUE){
        searchResults <- data.frame(species = species_list,
                                    url = url_list,
                                    stringsAsFactors = FALSE)
        
        message_text <- paste0("A total of ", n_species, " species links retrieved.")
        
      }else{
        searchResults <- species_list
        message_text <- paste0("A total of ", n_species, " species retrieved.")
      }
      if (showProgress) message("Data collection is done!\n", message_text, "\n")
      return(searchResults)
    }
  }else{
    species_list <- dataList$species
    genus_list <- sub(" .*", "", species_list)
    url_list <- dataList$url
    n_species <- length(species_list)
  }
  
  # taxonomicInfo == TRUE ---------------------------------------------------
  if (taxonomicInfo == TRUE) {
    if(showProgress == TRUE){
    message("Sampling species higher taxa progress:\n")
    }
    orders <- c("Squamata", "Crocodylia", "Rhynchocephalia", "Testudines")
    suborders <- c("Amphisbaenia","Sauria", "Serpentes")
    

      results_list <- safeParallel(
        data = species_list,
        FUN = function(x) higherSampleParallel(
          x,
          species_list = species_list,
          genus_list = genus_list,
          url_list = url_list,
          orders = orders,
          suborders = suborders,
          fullHigher = fullHigher,
          getLink = getLink
        ),
        cores = cores,
        showProgress = showProgress
      )
      results_list <- Filter(Negate(is.null), results_list)
      searchResults <- as.data.frame(dplyr::bind_rows(results_list))
      
# testing and warning for error messages ----------------------------------
      # Check which rows have errors flagged TRUE
      error_rows <- which(!is.na(searchResults$error) & searchResults$error == TRUE)
      
      if (length(error_rows) > 0) {
        # Extract species names and error messages for those rows
        species_with_errors <- searchResults$species[error_rows]
        messages <- searchResults$message[error_rows]
        
        n_errors <- length(species_with_errors)
        max_show <- 5
        
        # Construct warning message
        species_msgs <- paste0("- ", species_with_errors[1:min(max_show, n_errors)], ": ",
                              messages[1:min(max_show, n_errors)])
        
        if (n_errors > max_show) {
          species_msgs <- c(species_msgs, paste0("... and ", n_errors - max_show, " others"))
        }
          warning_msg <-paste0(
          "Data sampling completed with errors for the following species:\n",
          paste0(species_msgs, collapse = "\n"),
          "\n\nTo extract failed species from your original data, use:\n",
          "failed_spp <- df[df$species %in% df$species[df$error == TRUE], c('species', 'url')]\n",
          "Then ran reptSpecies(dataList = failed_spp)."
        )
        warning(warning_msg)
      }
      return(searchResults)
    
  } # <--- closes if (taxonomicInfo == TRUE)
} # <--- closes reptSpecies function
