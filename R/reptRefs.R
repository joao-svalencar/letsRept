#' Extract references from the Reptile Database
#'
#' @param x A binomial species name (e.g., "Boa constrictor").
#' @param getLink Logical; if TRUE, also returns associated links to references.
#'
#' @return A character vector of references (if getLink = FALSE), or a data frame with columns `reference` and `link`.
#' 
#' @examples
#' \donttest{
#' df <- reptRefs("Apostolepis adhara")
#' }
#' 
#' @export
reptRefs <- function(x = NULL, getLink = TRUE) {
  if (is.null(x) || !is.character(x)) {
    stop("x must be a binomial species name as a character string.")
  }
  
  parts <- strsplit(x, " ")[[1]]
  if (length(parts) != 2) {
    stop("x must be a binomial species name (e.g., 'Boa constrictor').")
  }
  
  genus <- parts[1]
  species <- parts[2]
  spp_url <- paste0("https://reptile-database.reptarium.cz/species?genus=", genus, "&species=", species)
  
  page <- rvest::read_html(spp_url)
  table_node <- rvest::html_element(page, "table")
  
  if (is.na(table_node)) {
    stop("Species not found: ", x)
  }
  
  rows <- xml2::xml_children(table_node)
  refs_node <- rows[grepl("References", rows)]
  
  ul_node <- rvest::html_element(refs_node, "td:nth-child(2) ul")
  li_nodes <- xml2::xml_children(ul_node)
  
  # Extract links if requested
  if (getLink) {
    ref_links <- sapply(li_nodes, function(li) {
      a_node <- xml2::xml_find_first(li, ".//a")
      if (!is.na(a_node) && length(a_node) > 0) {
        xml2::xml_attr(a_node, "href")
      } else {
        "no link available"
      }
    }, USE.NAMES = FALSE)
  }
  
  # Clean reference text (remove <a> before extracting)
  xml2::xml_remove(xml2::xml_find_all(li_nodes, ".//a"))
  ref_texts <- trimws(rvest::html_text(li_nodes, trim = TRUE))
  ref_texts <- sub("\\s*-\\s*$", "", ref_texts)
  ref_texts <- ref_texts[nzchar(ref_texts)]
  
  # Final output
  if (getLink) {
    if (length(ref_texts) != length(ref_links)) {
      stop("Mismatch between reference texts and links.")
    }
    return(data.frame(reference = ref_texts, link = ref_links, stringsAsFactors = FALSE))
  } else {
    return(ref_texts)
  }
}
