# letsRept

### **An Interface to the Reptile Database**

This package was developed to facilitate the processes of reptile nomenclature update based on a search for species synonyms according to [The Reptile Databse](https://reptile-database.reptarium.cz) website (Uetz et al., 2025).

Currently, the package access many species information from The Reptile Database using R interface.

I hope it to be useful, to people trying to match databases from different sources (IUCN, species traits database, etc), or trying to get summaries from a given higher taxa or region (e.g.: Snakes from Brazil). But it can also just print single species information directly in R.

Any feedback, suggestion or request are welcome!

### **Download**

To install the stable version of this package user must run:

```{.r}
# install.packages("devtools")
devtools::install_github("joao-svalencar/letsRept", ref="main")
library(letsRept)
```

### **List of functions and examples**
**Function `reptSearch`:**

Prints species information sampled from the respective species page in RD:
```{.r}
#single species:
reptSearch(binomial = "Apostolepis adhara")
```

**Function `reptAdvancedSearch`:**

Creates a link for a page as derived from an Advanced Search in RD (multiple species in a page):
```{.r}
#create multiple species link:
link <- reptAdvancedSearch(genus = "Apostolepis") #returns a link to access a list of all Apostolepis species
link <- reptAdvancedSearch(higher = "snakes", location = "Brazil") #returns a link to access a list of all snake species in Brazil
```

**Function `reptSpecies`:**

Sample species data from the species link created by `reptAdvancedSearch`. It includes higher taxa information, authors, year of description, and the species url.

- Returns higher taxa information and species url:
```{.r}
#sample multiple species data:
apo <- reptSpecies(link, taxonomicInfo = TRUE, getLink = TRUE) 
```
- Returns only species url - Faster and recommended for large datasets:
```{.r}
apo <- reptSpecies(link, taxonomicInfo = FALSE, getLink = TRUE)
```

**Function `reptSynonyms`:**

Samples species synonyms using a data frame with species names and the species link (e.g.: the result of `reptSpecies(link, getLink=TRUE)`).

⚠️ ATTENTION!⚠️ 

The complex `regex` pattern used to sample synonyms from The Reptile Database is quite efficient but still sample 0.2% of them in bad format. Most cases represent unusual nomenclature so users might not face any problems trying to match current valid names. In any case, I fixed (pottentially) all unusual synonym formats the internal dataset `allSynonyms` (last update: 23rd May, 2025)

```{.r}
#sample species synonyms
apo_syn <- reptSynonyms(apo)
```
**Function `reptSync`:**

Initially inspired in function `aswSync` from package [AmphiNom](https://github.com/hcliedtke/AmphiNom) (Liedtke, 2018).

This is the most recursive function of the package, using all the previous functions in order to provide the most likely updated nomenclature for the queried species.
The function is divided in two main steps. Here is how it works:

*Step 1*

The function queries a vector of species (e.g.: IUCN, or a regional list), check their validity through `reptSearch` and returns a data frame with current valid species names.
When `reptSearch` finds a species page it assumes that is the valid name for the queried species and returns the status "up_to_date".
When `reptSearch` doesn't find a species it parses the binomial to `reptAdvancedSearch` using the synonym filter.
If `reptAvancedSearch` returns a link for a species page that species name is considered valid for the synonym queried and the function returns the status "updated".
Otherwise, `reptAvancedSearch` will return a link for a page with a list of species, then the function assumes that the queried synonym could be assigned to any of those valid names and returns the status: "ambiguous".
If the queried species does not return a species page nor a page for multiple species the function returns to column "RDB" the sentence "Not found" and to column "status" the word "unknown".

*Step 2*

Step 2 is activated only if `solveAmbiguity = TRUE`.
When `reptAvancedSearch` returns a link for a page with a list of species, that link is parsed to `reptSpecies` which collects species names and `urls` and automatically parses the resulting data frame to `reptSynonyms`.
Finally, with the result of `reptSynonyms` the function compares the queried species with all listed synonyms.
If the queried species is actually listed as a synonym of only one of the searched species (e.g. the queried name is not a synonym, but is mentioned in the comments section), the function will return that valid name and status will be "updated".
If the queried species is actually a synonym of more than one valid species, then the function will return both species names and the status will still be "ambiguous".

See package vignettes for more details.

```{.r}
# comparing synonyms:
query <- c("Vieira-Alencar authoristicus",
           "Boa atlantica",
           "Boa diviniloqua",
           "Boa imperator",
           "Boa constrictor longicauda")

reptSync(query)

#example 2:
query <- c("Vieira-Alencar authorisensis",
           "Apostolepis ambiniger",
           "Apostolepis cerradoensis",
           "Elapomorphus assimilis",
           "Apostolepis tertulianobeui",
           "Apostolepis goiasensis")

reptSync(query)
```

**Function `reptSplitCheck`:**

Queries binomial names as synonyms using `reptAdvancedSearch`, and checks whether any associated species were described after a user-defined date (e.g., the publication date of the dataset being used).

See package vignettes for more details.

```{.r}
query <- c("Tantilla melanocephala",
           "Atractus snethlageae",
           "Oxybelis aeneus")

reptSplitCheck(query, pubDate = 2019) # pubDate of Nogueira et al., Atlas of Brazilian Snakes
```

**Function `reptTidySyn`:**

This function was developed exclusively to improve the visualization of `reptSync` outcome.
Queried species with many current valid names would break the data frame visualizarion in the R console.
`reptTidySyn` stacks current valid names and improves data visualization.
Moreover, the argument `filter`, allows users to filter the printed data frame by "status" so users can focus only in the status that they want to evaluate.

```{.r}
query <- c("Vieira-Alencar authorisensis",
           "Apostolepis ambiniger",
           "Apostolepis cerradoensis",
           "Elapomorphus assimilis",
           "Apostolepis tertulianobeui",
           "Apostolepis goiasensis")

df <- reptSync(query)
reptTidySyn(df)
```
### **Internal datasets**

- The package counts with a full list of current valid species (`allReptiles` - 12,440 species) with their respective higher taxa information (updated to 23th of May, 2025);

- A dataset with all unique synonyms for each current valid species (`allSynonyms` - 53,159 entries - updated to 23th of May, 2025); 

- Another synonyms dataset with all entries considering their respective references (`allSynonymsRef`110,413 entries - updated to 23rd of May, 2025).

### **Next steps**

- [x] &nbsp; Implement `reptAdvancedSearch` link testing and result summary
- [x] &nbsp; Implement `reptSynonym` batch sampling and tryCatch() mechanism
- [x] &nbsp; `reptSpecies()` with parallel sampling
- [x] &nbsp; `reptSynonyms()` with parallel sampling
- [x] &nbsp; CRAN submission (June 14th, 2025)
- [x] &nbsp; CRAN reviewed submission (June 18th, 2025)
- [x] &nbsp; CRAN release 0.1.0 (June 23rd, 2025)
- [ ] &nbsp; Paper submission (ongoing)
- [x] &nbsp; `reptSync` upgrade
- [x] &nbsp; Implement "up_to_date" check (`reptSplitCheck`)
- [x] &nbsp; Implement Reference sampling (df with links)
- [x] &nbsp; Implement adapted version of `AmphiNom::asw_stats()` (Liedtke, 2018)
- [ ] &nbsp; Implement adapted version of `AmphiNom::synonym_report()` (Liedtke, 2018)
- [ ] &nbsp; Implement IUCN match

### **How to Cite**

To cite this package in publications, run:

```r
citation("letsRept")
```

⚠️ Important note:

`letsRept` retrieves valuable taxonomic and synonymy data directly from [The Reptile Database](http://www.reptile-database.org)..
When citing this package, please also cite the original database as a data source.

### **References**
Liedtke, H. C. (2018). AmphiNom: an amphibian systematic tool. Systematics and Biodiversity, 17(1) 1-6. https://doi.org/10.1080/14772000.2018.1518935

Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database. http://www.reptile-database.org

### **Author:**

Vieira-Alencar, João Paulo dos Santos (joaopaulo.valencar@gmail.com)

[Orcid](https://orcid.org/0000-0001-6894-6773) | [Research Gate](https://www.researchgate.net/profile/Joao-Paulo-Alencar)

Ph.D in Ecology at USP

Post-Doc at:
Laboratório de Evolução e Diversidade I  - "LED 1" - Centro de Ciências Naturais e Humanas, Universidade Federal do ABC, São Bernardo do Campo, SP – Brazil
