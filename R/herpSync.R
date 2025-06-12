##########################################################################################################
################### function herpSync by:  HC LIEDTKE & JP VIEIRA-ALENCAR  ##############################
##########################################################################################################

#' Submit query to find updates in nomenclature
#'
#' This function takes a query (a list of taxon names to be assessed) and the amphibian species of the world synonym table (preferably generated with the function get_synonyms(), or loaded from the data set stored internally) as input and returns an updated list of names.
#'
#'
#' Two logical arguments can be turned on to a) allow an "on the fly" decision to be made on what name to take if synonym matches multiple names (if not, it will return all possible names) and to b) return the original query name if no match is found
#' @usage herpSync(query, synonym, interactive = FALSE, return.no.matches = FALSE)
#' 
#' @param query vector of taxon names to be processed (can also be tip labels of a phylogeny for example)
#' @param synonym Reptile Database synonym reference table on which to base new names on Default setting will use the internally stored data set that may not be the most up-to-date.
#' @param interactive logical argument (default=FALSE) of whether to allow an "on the fly" decision to be made on what name to take if synonym matches multiple names. FLASE will return all possible names for a given query as a string, TRUE will ask the user to select one.
#' @param return.no.matches logical argument (default=FALSE) of whether to leave taxa not found in the reference table blank or whether to fill in the names provided by the query.
#' 
#' @return this function returns a data frame with the following information/columns: original/input names, "stripped" names with no formatting, status of what action has been taken, updated names as recommended by the reference table
#' 
#' @references
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @examples
#' boaLink <- herpAdvancedSearch(genus = "Boa")
#' boa <- herpSpecies(boaLink, getLink=TRUE, taxonomicInfo = FALSE)
#' boa_syn <- herpSynonyms(boa, getRef = FALSE, cores = 1)
#' query <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
#' herpSync(query, boa_syn)
#' 
#' @export
#' 

herpSync<-function(query, synonym=NULL, interactive=FALSE, return.no.matches=FALSE){

  ### first step is to remove all formatting from both names and the frost database
  if(is.null(synonym))
  {
    synonym <- letsHerp::allSynonyms
    cat(" User's personal synonym list not provided using internal database" )
  }
  else{
  synonym <- synonym
  }
  
  query<-data.frame(query=query,stringsAsFactors=F)
  query$stripped<-gsub(query$query, pattern=" ",replacement="_")
  query$stripped<-tolower(query$stripped)
  
  synonym$stripped<-gsub(synonym$synonym, pattern=" ",replacement="_", )
  synonym$stripped<-tolower(synonym$stripped)
  
  ###### then:
  
  #update names. if interactive=F then it will return a string of names if names are ambiguous
  query$status<-NA
  query$warnings<-NA
  query$Reptile_Database<-NA
  
  if(!interactive){
    for(i in 1:nrow(query)){
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))==1){
        query$Reptile_Database[i]<-as.character((synonym$species[synonym$stripped==query$stripped[i]])[1])
        query$status[i]<-"updated"
      }
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))>1){
        query$status[i]<-"ambiguous"
        query$Reptile_Database[i]<-paste(unique(synonym$species[synonym$stripped==query$stripped[i]]), collapse = ", ")
      }
    }
  }
  
  # if interactive=T, the user can resolve each case of ambiguity on the go
  
  if(interactive){
    for(i in 1:nrow(query)){
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))==1){
        query$Reptile_Database[i]<-as.character((synonym$species[synonym$stripped==query$stripped[i]])[1])
        query$status[i]<-"updated"
      }
      if(length(unique(synonym$species[synonym$stripped==query$stripped[i]]))>1){
        n <- readline(prompt=cat("",as.character(query$query[i]),"can be matched with multiple species names. Choose one of the following by entering the line number in the console:","",as.character(unique(synonym$species[synonym$stripped==query$stripped[i]])), sep="\n"))
        query$Reptile_Database[i]<-as.character(unique(synonym$species[synonym$stripped==query$stripped[i]])[as.integer(n)])
        query$status[i]<-"updated"
      }
    }
  }
  ### update the status column to show which query names were actually already up to date (i.e. matching frost)
  uetz.stripped<-tolower(query$Reptile_Database)
  uetz.stripped<-gsub(uetz.stripped, pattern=" ",replacement="_")
  for(i in 1:nrow(query)){
    if(!is.na(uetz.stripped[i]) & query$stripped[i]==uetz.stripped[i]){
      query$status[i]<-"up_to_date"
    }
  }
  
  ##find names which are not listed in frost:
  if(!return.no.matches){
    query[which(!query$stripped %in% synonym$stripped),"status"]<-"name_not_found"
    query[which(!query$stripped %in% synonym$stripped),"Reptile_Database"]<-NA
  }
  if(return.no.matches){
    query[which(!query$stripped %in% synonym$stripped),"status"]<-"name_not_found"
    query[query$status=="name_not_found","Reptile_Database"]<-as.character(query[query$status=="name_not_found","query"])
  }
  
  ##indicate which species names are duplicated as a result of the renaming:
  duplicates<-unique(query$Reptile_Database[duplicated(query$Reptile_Database)])
  query$warnings[query$Reptile_Database %in% duplicates[!is.na(duplicates)]]<-"duplicated"
  
  ##formate the output names the same way as the input names
  for(i in 1:nrow(query)){
    if(grepl(query$query[i], pattern="_")){
      query$Reptile_Database[i]<-gsub(query$Reptile_Database[i], pattern="([[:alpha:]]) ([[:alpha:]])", replacement="\\1_\\2")
    }
  }
  
  query$query<-as.character(query$query)
  query <- query[,-2]
  return(query)
}
