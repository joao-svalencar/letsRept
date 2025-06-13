##########################################################################################################
####################### function herpSynonyms by:  JP VIEIRA-ALENCAR  ####################################
##########################################################################################################

#' Get reptile species synonyms
#' 
#' @description
#' creates a data frame containing a list of reptile species current valid names according to The Reptile Database alongside with all their recognized synonyms
#' 
#' @usage herpSynonyms(x,
#'                     getRef = FALSE,
#'                     checkpoint = NULL,
#'                     backup_file = NULL,
#'                     resume=FALSE,
#'                     cores = max(1, parallel::detectCores() - 1))
#' 
#' @param x A data frame with columns: 'species' and 'url' (their respective Reptile Database url).
#' Could be the output of letsHerp::herpSpecies().
#' @param getRef A logical value. If TRUE, returns synonyms with the respective references that mention them. default = *FALSE*
#' @param checkpoint An integer representing the number of species to process before saving progress to the backup file.
#' Helps prevent data loss in case the function stops unexpectedly. Backups are saved only if checkpoint is not NULL.
#' OBS.: checkpoint are not used under parallel processing, if mandatory choose cores = 1 (safe, but decreases processing performance).
#' @param backup_file A character string with the path to access, read and save the backup file.
#' @param resume A logical value. If TRUE, takes the path to backup_file and resume sampling from the last backup file.
#' OBS.: backup_file are not created under parallel processing, if mandatory choose cores = 1 (safe, but decreases processing performance).
#' @param cores number of cores dedicated to parallel processing
#' 
#' @returns 'herpSynonyms' returns a data frame with columns: species and their respective synonyms according to the version of Reptile Database. Optionally, returns the references that mentioned each synonym.
#' 
#' @references 
#' Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database, http://www.reptile-database.org
#' Liedtke, H. C. (2018). AmphiNom: an amphibian systematics tool. *Systematics and Biodiversity*, 17(1), 1–6. https://doi.org/10.1080/14772000.2018.1518935
#' 
#' @examples
#' boa <- letsHerp::allReptiles[grep("^Boa\\s", allReptiles$species),]
#' boa_syn <- herpSynonyms(boa, getRef = FALSE, cores=2) #only synonyms
#' 
#' @export
#'

herpSynonyms <- function(x, getRef=FALSE, checkpoint = NULL, backup_file = NULL, resume=FALSE, cores = max(1, parallel::detectCores() - 1))
{
  if (cores==1 && is.null(backup_file) && !is.null(checkpoint) && checkpoint < length(x$species)) {
    stop("You must provide a valid backup_file path if checkpoint is smaller than the number of species.")
  }else 
    if(!is.null(backup_file) && !grepl(".rds", backup_file)){
      stop("Backup file path must end with 'filename.rds'")
    }

  if(cores > 1){
    synonyms <- safeParallel(
      data = seq_along(x$species),
      FUN = function(i) getSynonymsParallel(
        i,
        x = x,
        getRef = getRef
        ),
      cores = cores
    )
    df <- do.call(rbind, synonyms)
    
    if (!getRef) {
      df$combined <- paste(df$species, df$synonyms, sep = "_")
      df <- data.frame(unique(df$combined))
      df <- tidyr::separate(df, col = "unique.df.combined.", into = c("species", "synonym"), sep = "_", convert = TRUE)
      cat("\nSynonyms sampling complete!\n")
    }
    return(df)
  }else{
synonyms <- getSynonyms(x=x, checkpoint = checkpoint, resume = resume, backup_file = backup_file, getRef = getRef)
  }
  return(synonyms)
}
