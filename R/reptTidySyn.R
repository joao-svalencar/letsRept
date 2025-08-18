#' Printing reptSync and reptSplitCheck outcomes in a tidy way
#'
#' @description
#' Prints the data frame derived from \code{reptSync} or \code{reptSplitCheck} in a tidy way.
#' Optionally, it filters the data frame for species with unresolved nomenclature only.
#'
#' @param df The data frame derivated from \code{reptSync}
#' @param filter Logical. If \code{TRUE} will print only the species entries with unresolved nomenclature (e.g.: ambiguous or not_found). Default is \code{TRUE}
#'
#' @return Invisibly returns `NULL`. Used for side-effect printing only.
#'
#' @examples
#' df <- data.frame(
#'   species = c("Genus epithet 1", 
#'               "Genus epithet 2",
#'               "Genus epithet 3",
#'               "Genus epithet 4",
#'               "Genus epithet 5"),
#'   synonyms = c("Genus epithet 1.1; Genus epithet 1.2",
#'                 "Genus epithet 2",
#'                 "Genus epithet 3",
#'                 "Not found",
#'                 "Genus epithet 5.1; Genus epithet 5.2; Genus epithet 5.3"),
#'   status = c("ambiguous", 
#'              "updated",
#'              "up_to_date",
#'              "not_found",
#'              "ambiguous"),
#'   stringsAsFactors = FALSE
#' )
#' reptTidySyn(df, filter = c("ambiguous", "not_found"))
#'
#' @export
reptTidySyn <- function(df, filter = NULL) {
  if (!is.data.frame(df) || ncol(df) < 2) {
    stop("Input must be a data frame with at least two columns.")
  }
  
  if ("url" %in% names(df)) df <- df[ , !(names(df) == "url")]
  
  statuses <- c("up_to_date", "updated", "ambiguous", "not_found", "synonymization", "check_split")
  
  if (is.null(filter)) filter <- statuses
    
    df <- df[df$status %in% filter,]

  spacer <- "   "  # 3 spaces between columns
  col_names <- names(df)
  
  # Determine column widths
  col_widths <- sapply(seq_along(df), function(i) {
    vals <- if (i == 1) df[[i]] else unlist(strsplit(df[[i]], ";\\s*"))
    max(nchar(c(col_names[i], vals)), na.rm = TRUE)
  })
  
  # Total line width
  total_width <- sum(col_widths) + (length(col_widths) - 1) * nchar(spacer)
  
  # Print header
  header_line <- paste(
    mapply(function(name, width) sprintf("%-*s", width, name), col_names, col_widths),
    collapse = spacer
  )
  cat(header_line, "\n")
  cat(strrep("-", total_width), "\n")
  
  # Print each row
  for (i in seq_len(nrow(df))) {
    # Split all columns except first into lists
    value_lists <- lapply(df[-1], function(col) strsplit(col[i], ";\\s*")[[1]])
    max_lines <- max(sapply(value_lists, length), 1)
    
    for (j in seq_len(max_lines)) {
      row_output <- c()
      
      for (k in seq_along(df)) {
        if (k == 1) {
          # First column: only show species name in the first line
          row_output[k] <- if (j == 1) sprintf("%-*s", col_widths[k], df[[k]][i])
          else sprintf("%-*s", col_widths[k], "")
        } else {
          val <- value_lists[[k - 1]]
          this_val <- if (j <= length(val)) val[j] else ""
          row_output[k] <- sprintf("%-*s", col_widths[k], this_val)
        }
      }
      
      cat(paste(row_output, collapse = spacer), "\n")
    }
  }
  
  invisible(NULL)
}
