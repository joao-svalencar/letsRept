#' Search for a Single Reptile Species in The Reptile Database (RDB)
#' 
#' @description
#' Queries The Reptile Database (RDB) for information about a single reptile species using its binomial name.
#' 
#' @usage reptSearch(binomial=NULL, getRef=FALSE, verbose=TRUE)
#' 
#' @param binomial Character string. The valid binomial name of a reptile species (e.g., "Boa constrictor").
#' @param getRef Logical. If \code{TRUE}, returns the list of references from RDB associated with the species. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints species information in the console. Default is \code{TRUE}.
#'
#' @return
#' A list containing species information retrieved from The Reptile Database. If \code{getRef = TRUE}, returns references related to the species.
#' 
#' @examples
#' \donttest{
#' reptSearch("Boa constrictor")
#' reptSearch("Boa constrictor", getRef = TRUE)
#' }
#' 
#' @seealso \code{\link{reptSynonyms}}, \code{\link{reptSpecies}} for related species data functions.
#' @references
#' Uetz, P., Freed, P., & Hošek, J. (Eds.). (2025). The Reptile Database. Retrieved from \url{http://www.reptile-database.org}
#' 
#' @export

reptSearch <- function(binomial = NULL, getRef = FALSE, verbose = TRUE) {
  if (!is.null(binomial)) {
    output_list <- list()
    base_url <- "https://reptile-database.reptarium.cz/species"
    gen <- strsplit(binomial, " ")[[1]][1]
    species <- strsplit(binomial, " ")[[1]][2]
    query <- paste0("?genus=", gen, "&species=", species)
    sppLink <- paste0(base_url, query)
    
    #url <- rvest::read_html(sppLink)
    url <- safeRequest(sppLink)
    element <- rvest::html_element(url, "table") # full table
    
    if (is.na(element)) {
      if(verbose) message("Species not found: ", binomial, "\nSearching as synonym in advanced search.\n")
      link <- reptAdvancedSearch(synonym = binomial, verbose = verbose)
      
      if(is.null(link)){
          return(invisible(NULL))
      }else{
          return(link)
      }
    }
    
    output_list[["url"]] <- sppLink
    output_list[["species"]] <- binomial
    
    if(verbose==TRUE){
    title_node <- rvest::html_element(url, "h1") #species + authors
    title_text <- rvest::html_text(title_node, trim = TRUE) #species + authors
    cat("Species:\n -", title_text, "\n\n")  #improve with author names
    }
    
    rows <- xml2::xml_children(element)
    for (row in rows) {
      cells <- xml2::xml_children(row)
      if (length(cells) < 2) next
      
      title <- rvest::html_text(cells[1], trim = TRUE)
      title_clean <- gsub(":$", "", title)
      
      if (tolower(title_clean) == "external links") {
        next  # Always skip external links
      }
      
      if (title_clean == "References") {
        if (getRef) {
          ref_list <- NULL
          li_nodes <- rvest::html_elements(cells[[2]], "li")
          xml2::xml_remove(xml2::xml_find_all(li_nodes, ".//a"))
          ref_list <- trimws(rvest::html_text(li_nodes, trim = TRUE))
          ref_list <- sub("\\s*-\\s*$", "", ref_list)
          ref_list <- ref_list[nzchar(ref_list)]
          output_list[["References"]] <- ref_list
        }
        next  # Handle references later
      }
      
      # General case: inject <SPLIT> after <br> and split
      xml2::xml_add_sibling(
        rvest::html_nodes(cells[[2]], "br"), "marker", "<SPLIT>"
      )
      content_raw <- rvest::html_text(cells[2], trim = TRUE)
      content_split <- trimws(unlist(strsplit(content_raw, "<SPLIT>")))
      content_split <- content_split[nzchar(content_split)]
      
      if (length(content_split) > 0 && any(nzchar(content_split))) {
        output_list[[title_clean]] <- content_split
        if(verbose == TRUE){
        cat(paste0(title_clean, ":\n"))
        for (item in content_split) {
          if (nzchar(item)) {
            cat(" -", item, "\n")
            }
          }
        cat("\n")
        }
      } else if (nzchar(content_raw)) {
        output_list[[title_clean]] <- content_raw
        if(verbose == TRUE){
        cat(paste0(title_clean, ":\n", content_raw, "\n\n"))
        }
      }
      # else: content is empty, skip printing and don't add to output_list
    }
    
    # Print and add references only at the end, if requested
    if (verbose==TRUE && getRef && !is.null(ref_list)) {
      cat("References:\n")
      for (item in ref_list) cat(" -", item, "\n")
      cat("\n")
    }
    return(invisible(output_list))
  }
}

