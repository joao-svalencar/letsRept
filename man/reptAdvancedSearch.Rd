% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reptAdvancedSearch.R
\name{reptAdvancedSearch}
\alias{reptAdvancedSearch}
\title{Search The Reptile Database website (RDB): Advanced}
\usage{
reptAdvancedSearch(
  higher = NULL,
  genus = NULL,
  year = NULL,
  common_name = NULL,
  synonym = NULL,
  location = NULL,
  verbose = TRUE,
  exact = FALSE
)
}
\arguments{
\item{higher}{Character string. A higher-level reptile taxon above genus (e.g., \code{"snakes"} or \code{"Boidae"}).}

\item{genus}{Character string. The current valid name of a reptile genus (e.g., \code{"Apostolepis"}).}

\item{year}{Character string. Filters the search by year of species description (e.g., \code{"2025"}).}

\item{common_name}{Character string. A common name potentially linked to a species or genus (e.g., \code{"tree boa"}).}

\item{synonym}{Character string. A name potentially regarded as a synonym of a valid taxon (e.g., \code{"Boa diviniloqua"}).}

\item{location}{Character string. A country or region name used to list species expected to occur there.}

\item{verbose}{Logical. To be passed to \code{reptSpecies()} in the case of a provided synonym corresponds unambiguously to a valid species.
If \code{TRUE}, prints status messages and species information in the console. Default is \code{TRUE}.}

\item{exact}{Logical. To return outputs that matches exactly the searched term (e.g., avoid returning genus "Boaedon" when searching for "Boa"). Default is \code{FALSE}.}
}
\value{
A character string containing the URL to be used in \code{\link{reptSpecies}}.

If a provided synonym corresponds unambiguously to a valid species, the function also prints species information retrieved from RDB to the console.
}
\description{
Creates a search URL for retrieving species lists from RDB based on multiple filters.
This URL is primarily used by \code{\link{reptSpecies}}, but can also be used manually for advanced queries.

If a synonym is provided and can be unambiguously matched to a valid species, the function also prints detailed information for that species.
}
\note{
The argument \code{exact} does not work properly for searches using logical arguments (e.g. \code{AND/OR}).
If you want to force an exact match (e.g., \code{"Boa"} as a phrase) with multiple terms (e.g., \code{"Boa OR Apostolepis"}),
you must manually include quotes in the input string, e.g., \code{"\"Boa\" OR Apostolepis"}.

Logical operators (e.g., \code{OR}, \code{AND}) are supported and will be properly formatted in the search.
To exclude terms, use a leading minus sign (e.g., \code{higher = "-snakes"}) following RDB's query syntax, instead of using \code{NOT}.

When a synonym is matched to a single valid species, the function will also display the species' full information as a side effect.
}
\examples{
\donttest{
reptAdvancedSearch(higher = "snakes", year = "2010", location = "Brazil")
reptAdvancedSearch(year = "2010 OR 2011 OR 2012")
reptAdvancedSearch(genus = "Apostolepis OR \"Boa\" OR Atractus") #quotes "Boa"
}
}
