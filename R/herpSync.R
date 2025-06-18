#' Synchronize species names using synonym reference table
#'
#' @description
#' Compares a user-provided list of reptile taxon names against a synonym table from The Reptile Database and returns an updated list of valid names.
#' Particularly useful to standardize names before analyses or when integrating heterogeneous taxonomic sources.
#'
#' Supports interactive disambiguation in cases where multiple valid names are found for a given synonym. Optionally, unmatched names can be retained or returned as blank.
#'
#' @usage
#' herpSync(query, synonym, interactive = FALSE, return.no.matches = FALSE)
#'
#' @param query A character vector of taxon names to be processed (e.g., species list, phylogenetic tip labels, or trait table entries).
#' @param synonym A data frame with a synonym reference table (e.g., output from \code{\link{herpSynonyms}}). If not provided, the function uses the internal dataset \code{letsHerp::allSynonyms}, which may not be the most up-to-date.
#' @param interactive Logical. If \code{TRUE}, the function allows real-time selection when a synonym matches multiple valid names. If \code{FALSE} (default), all possible matches are returned as concatenated strings.
#' @param return.no.matches Logical. If \code{TRUE}, species not found in the synonym table will be returned as originally provided in the query. If \code{FALSE} (default), unmatched entries will be left blank.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{input}: original input names from the query.
#'   \item \code{stripped}: standardized versions of the input names (e.g., without authors or formatting).
#'   \item \code{status}: description of the outcome (e.g., \code{"updated"}, \code{"not found"}, \code{"multiple matches"}).
#'   \item \code{updated}: best-matching valid names according to the synonym table.
#' }
#' @note
#' The internally stored synonym table \code{letsHerp::allSynonyms} was last updated on May 23rd, 2025.
#' 
#' @references
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#'
#' @examples
#' boa_syn <- letsHerp::allSynonyms[grep("^Boa\\s", allSynonyms$species), ]
#' query <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
#' herpSync(query, boa_syn)
#'
#' @export
#' 

herpSync<-function(query, synonym=NULL, interactive=FALSE, return.no.matches=FALSE){

  ### first step is to remove all formatting from both names and The Reptile Database
  if(is.null(synonym))
  {
    synonym <- letsHerp::allSynonyms
    cat(" User's personal synonym list not provided using internal database" )
  }
  else{
  synonym <- synonym
  }
  
  query <- data.frame(query=query, stringsAsFactors=F)
  query$stripped <- gsub(query$query, pattern=" ", replacement="_")
  query$stripped <- tolower(query$stripped)
  
  synonym$stripped <- gsub(synonym$synonym, pattern=" ", replacement="_")
  synonym$stripped <- tolower(synonym$stripped)
  
  ###### then:
  
  #update names. if interactive = FALSE then it will return a string of names if names are ambiguous
  query$status <- NA
  query$warnings <- NA
  query$Reptile_Database <- NA
  
  if(!interactive){
    for(i in 1:nrow(query)){
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))==1){
        query$Reptile_Database[i] <- as.character((synonym$species[synonym$stripped==query$stripped[i]])[1])
        query$status[i] <- "updated"
      }
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))>1){
        query$status[i] <- "ambiguous"
        query$Reptile_Database[i] <- paste(unique(synonym$species[synonym$stripped==query$stripped[i]]), collapse = ", ")
      }
    }
  }
  
  # if interactive = TRUE, the user can resolve each case of ambiguity on the go
  
  if(interactive){
    for(i in 1:nrow(query)){
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))==1){
        query$Reptile_Database[i]<-as.character((synonym$species[synonym$stripped==query$stripped[i]])[1])
        query$status[i]<-"updated"
      }
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))>1){
        n <- readline(prompt=cat("", as.character(query$query[i]), "can be matched with multiple species names. Choose one of the following by entering the line number in the console:", "", as.character(unique(synonym$species[synonym$stripped==query$stripped[i]])), sep="\n"))
        query$Reptile_Database[i] <- as.character(unique(synonym$species[synonym$stripped==query$stripped[i]])[as.integer(n)])
        query$status[i]<-"updated"
      }
    }
  }
  
  ### update the status column to show which query names were actually already up to date (i.e. matching The Reptile Database)
  rdb.stripped <- tolower(query$Reptile_Database)
  rdb.stripped<-gsub(rdb.stripped, pattern=" ",replacement="_")
  for(i in 1:nrow(query)){
    if(!is.na(rdb.stripped[i]) & query$stripped[i]==rdb.stripped[i]){
      query$status[i]<-"up_to_date"
    }
  }
  
  ##find names which are not listed in The Reptile Database:
  if(!return.no.matches){
    query[which(!query$stripped %in% synonym$stripped), "status"] <- "name_not_found"
    query[which(!query$stripped %in% synonym$stripped), "Reptile_Database"] <- NA
  }
  if(return.no.matches){
    query[which(!query$stripped %in% synonym$stripped), "status"] <- "name_not_found"
    query[query$status=="name_not_found", "Reptile_Database"] <- as.character(query[query$status=="name_not_found", "query"])
  }
  
  ##indicate which species names are duplicated as a result of the renaming:
  duplicates <- unique(query$Reptile_Database[duplicated(query$Reptile_Database)])
  query$warnings[query$Reptile_Database %in% duplicates[!is.na(duplicates)]] <- "duplicated"
  
  ##format the output names the same way as the input names
  for(i in 1:nrow(query)){
    if(grepl(query$query[i], pattern="_")){
      query$Reptile_Database[i] <- gsub(query$Reptile_Database[i], pattern="([[:alpha:]]) ([[:alpha:]])", replacement="\\1_\\2")
    }
  }
  
  query$query <- as.character(query$query)
  query <- query[,-2]
  return(query)
}
