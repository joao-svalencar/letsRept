#' Example dataset: letsRept_ReptTraits
#'
#' This dataset is a version of ReptTraits (Oskyrko et al. 2024) with two additional columns.
#'
#' @format A dataframe with 12,060 rows and 50 variables. The first three columns are:
#' \describe{
#'    \item{species}{Species name as in the original ReptTraits database}
#'    \item{RDB}{Current valid name according to the May 2025 version of the Reptile Database}
#'    \item{nomenclature.status}{Status from reptSync and reptSplitCheck. Additional status are: "extinct" and "manual_fix"}

#' }
#' @source The original data source is from Oskyrko et al. (2024); The new nomenclature in the RDB column was collected from the Reptile Database website https://reptile-database.reptarium.cz using functions reptSync and reptSplitCheck
#' 
"letsRept_ReptTraits"
