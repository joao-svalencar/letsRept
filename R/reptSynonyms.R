#' Retrieve Synonyms for Reptile Species from RDB
#' 
#' @description
#' Retrieves a data frame containing the current valid names of reptile species along with all their recognized synonyms, as listed in The Reptile Database (RDB). 
#' Optionally, the references citing each synonym can also be included.
#' 
#' @param x A character string with a species binomial or a data frame with columns \code{species} and \code{url}, typically the output of \code{\link{reptSpecies}} with \code{getLink = TRUE}.
#' @param getRef Logical. If \code{TRUE}, includes the reference(s) in which each synonym was mentioned. Default is \code{FALSE}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' @param checkpoint Optional. Integer specifying the number of species to process before saving a temporary backup. Backup is only saved if \code{cores = 1}. If set to \code{1}, saves progress after each species (safest but slowest).
#' @param backup_file Optional. Character string specifying the path to an \code{.rds} file used to save or resume intermediate results. Required if using \code{checkpoint} or \code{resume}.
#' @param resume Logical. If \code{TRUE}, resumes sampling from a previous run using the file provided in \code{backup_file}. Only works when \code{cores = 1}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is half of available cores (min = 1).
#'
#' @return
#' A data frame with columns:
#' \itemize{
#'   \item \code{species}: The valid species name according to RDB.
#'   \item \code{synonym}: A recognized synonym for the species.
#'   \item \code{reference} (optional): If \code{getRef = TRUE}, the citation where the synonym was reported.
#' }
#' 
#' @note
#' To enable safe resuming or backup progress saving, set \code{cores = 1}. Parallel processing does not support backups.
#'
#' @references 
#' Uetz, P., Freed, P., Aguilar, R., Reyes, F., Kudera, J., & Hošek, J. (eds.) (2025). The Reptile Database. Retrieved from \url{http://www.reptile-database.org}  
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. \emph{Systematics and Biodiversity}, 17(1), 1–6. \doi{10.1080/14772000.2018.1518935}
#'
#' @examples
#' # Filter species belonging to genus Boa
#' boa <- letsRept::allReptiles[grep("^Boa\\s", letsRept::allReptiles$species), ]
#' 
#' \donttest{
#' # Retrieve synonyms (without references)
#' boa_syn <- reptSynonyms(boa, getRef = FALSE, cores = 2)
#' Bconstrictor_syn <- reptSynonyms(x = "Boa constrictor")
#' }
#' 
#' @seealso \code{\link{reptSpecies}}, \code{\link{reptAdvancedSearch}}
#'
#' @export


reptSynonyms <- function(x, 
                         getRef=FALSE,
                         showProgress = TRUE,
                         checkpoint = NULL,
                         backup_file = NULL,
                         resume=FALSE,
                         cores = max(1L, floor(parallel::detectCores() / 2)))
{
  if(is.character(x)){
    species_list <- c()
    synonyms_list <- c()
    synonymsRef_list <- c()
    binomial <- x
    base_url <- "https://reptile-database.reptarium.cz/species"
    gen <- strsplit(binomial, " ")[[1]][1]
    species <- strsplit(binomial, " ")[[1]][2]
    query <- paste0("?genus=", gen, "&species=", species)
    sppLink <- paste0(base_url, query)
    
    url <- rvest::read_html(httr::GET(sppLink, httr::user_agent("Mozilla/5.0")))
    element <- rvest::html_element(url, "table") # species table
    
    # Extract synonyms
    syn <- xml2::xml_child(element, 4)
    td2 <- rvest::html_element(syn, "td:nth-child(2)")
    children <- xml2::xml_contents(td2)
    
    synonyms_vector <- unique(rvest::html_text(children[xml2::xml_name(children) == "text"], trim = TRUE))
    synonyms_vector <- synonyms_vector[!is.na(synonyms_vector) & trimws(synonyms_vector) != ""]
    
    synonyms <- clean_species_names(synonyms_vector)
    species <- rep(binomial, times = length(synonyms))
    
    species_list <- c(species_list, species)
    synonyms_list <- c(synonyms_list, synonyms)
    synonymsRef_list <- c(synonymsRef_list, synonyms_vector)
    
    if(getRef==TRUE){
      synonymsResults <- data.frame(species = species_list,
                                    synonyms = synonyms_list,
                                    ref = synonymsRef_list,
                                    stringsAsFactors = FALSE)
      return(synonymsResults)
    }else{
      synonymsResults <- data.frame(species = species_list,
                                    synonyms = synonyms_list,
                                    stringsAsFactors = FALSE)
      
      synonymsResults$combined <- paste((synonymsResults)$species, (synonymsResults)$synonyms, sep="_")
      
      uniquerec <- data.frame(unique(synonymsResults$combined))
      
      uniqueSynonyms <- tidyr::separate(data=uniquerec, col="unique.synonymsResults.combined.", 
                                        into=c("species", "synonyms"), sep="_",
                                        convert=TRUE)
      
      if(showProgress == TRUE){
        cat("\nSynonyms sampling complete!\n")
      }
      return(uniqueSynonyms)
    }
  }
  
  if (cores==1 && is.null(backup_file) && !is.null(checkpoint) && checkpoint < length(x$species)) {
    stop("You must provide a valid backup_file path if checkpoint is smaller than the number of species.")
  }else 
    if(!is.null(backup_file) && !grepl(".rds", backup_file)){
      stop("Backup file path must end with 'filename.rds'")
    }
  if (!"url" %in% names(x) || all(is.na(x$url))) {
    stop("No valid species URL found in x")
  }
  
  if(cores > 1){
    synonyms <- safeParallel(
      data = seq_along(x$species),
      FUN = function(i) getSynonymsParallel(
        i,
        x = x,
        getRef = getRef
        ),
      cores = cores,
      showProgress = showProgress
    )
    df <- do.call(rbind, synonyms)
    
    if (!getRef) {
      df$combined <- paste(df$species, df$synonyms, sep = "_")
      df <- data.frame(unique(df$combined))
      df <- tidyr::separate(df, col = "unique.df.combined.", into = c("species", "synonyms"), sep = "_", convert = TRUE)
      
      # warning for failed species
      n_failed <- sum(df$synonyms == "failed")
      
      if (n_failed > 0) {
        failed_spp <- unique(df$species[df$synonyms == "failed"])
        msg <- sprintf(
          "%d species failed. Retrieve failed species and URLs using:\nx[x$species %%in%% c(%s), c(\"species\", \"url\")]",
          n_failed,
          paste(shQuote(failed_spp), collapse = ", ")
        )
        warning(msg, call. = FALSE)
      }
      if(showProgress == TRUE){
      cat("\nSynonyms sampling complete!\n")
      }
    }
    return(df)
  }else{
    df <- getSynonyms(x=x,
                      checkpoint = checkpoint,
                      resume = resume,
                      backup_file = backup_file,
                      getRef = getRef,
                      showProgress = showProgress)
    
    # warning for failed species
    n_failed <- sum(df$synonyms == "failed")
    
    if (n_failed > 0) {
      failed_spp <- unique(df$species[df$synonyms == "failed"])
      msg <- sprintf(
        "%d species failed. Retrieve failed species and URLs using:\nx[x$species %%in%% c(%s), c(\"species\", \"url\")]",
        n_failed,
        paste(shQuote(failed_spp), collapse = ", ")
      )
      warning(msg, call. = FALSE)
    }
    return(df)
  }
}
