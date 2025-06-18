#' Retrieve Synonyms for Reptile Species from RDB
#' 
#' @description
#' Retrieves a data frame containing the current valid names of reptile species along with all their recognized synonyms, as listed in The Reptile Database (RDB). 
#' Optionally, the references citing each synonym can also be included.
#' 
#' @usage herpSynonyms(x,
#'                     getRef = FALSE,
#'                     showProgress = TRUE,
#'                     checkpoint = NULL,
#'                     backup_file = NULL,
#'                     resume = FALSE,
#'                     cores = max(1, parallel::detectCores() - 1))
#' 
#' @param x A data frame with columns \code{species} and \code{url}, typically the output of \code{\link{herpSpecies}} with \code{getLink = TRUE}.
#' @param getRef Logical. If \code{TRUE}, includes the reference(s) in which each synonym was mentioned. Default is \code{FALSE}.
#' @param showProgress Logical. If \code{TRUE}, prints data sampling progress. Default is \code{TRUE}.
#' @param checkpoint Optional. Integer specifying the number of species to process before saving a temporary backup. Backup is only saved if \code{cores = 1}. If set to \code{1}, saves progress after each species (safest but slowest).
#' @param backup_file Optional. Character string specifying the path to an \code{.rds} file used to save or resume intermediate results. Required if using \code{checkpoint} or \code{resume}.
#' @param resume Logical. If \code{TRUE}, resumes sampling from a previous run using the file provided in \code{backup_file}. Only works when \code{cores = 1}.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is one less than the total available cores.
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
#' boa <- letsHerp::allReptiles[grep("^Boa\\s", letsHerp::allReptiles$species), ]
#' 
#' \donttest{
#' # Retrieve synonyms (without references)
#' boa_syn <- herpSynonyms(boa, getRef = FALSE, cores = 2)
#' }
#' 
#' @seealso \code{\link{herpSpecies}}, \code{\link{herpAdvancedSearch}}
#'
#' @export


herpSynonyms <- function(x, getRef=FALSE, showProgress = TRUE, checkpoint = NULL, backup_file = NULL, resume=FALSE, cores = max(1, parallel::detectCores() - 1))
{
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
