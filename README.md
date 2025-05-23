---
output: github_document
---
# letsHerp
### **Author:**

Vieira-Alencar, João Paulo dos Santos (joaopaulo.valencar@gmail.com)

[Orcid](https://orcid.org/0000-0001-6894-6773)

[Research Gate](https://www.researchgate.net/profile/Joao-Paulo-Alencar)

Ph.D in Ecology at USP

Post-Doc at:
Laboratório de Evolução e Diversidade I  - "LED 1"

Centro de Ciências Naturais e Humanas, Universidade Federal do ABC, São Bernardo do Campo, SP – Brazil

### **An R Interface to the Reptile Database**

This package was developed to facilitate the processes of reptile nomenclature update based on a search for species synonyms according to [The Reptile Databse](https://reptile-database.reptarium.cz) website (Uetz et al., 2025).

Currently, the package access many species information from The Reptile Database using R interface.

I hope it to be useful, to people trying to match databases from different sources (IUCN, species traits database, etc), or trying to get summaries from a given higher taxa or region (e.g.: Snakes from Brazil). But it can also just print single species information directly in R.

Any feedback, suggestion or request are welcome!

### **Download**

To install the stable version of this package user must run:

```{.r}
# install.packages("devtools")
devtools::install_github("joao-svalencar/letsHerp", ref="main")
library(letsHerp)
```

### **List of functions and examples**
**Function `herpSearch`:**

Prints species information sampled from the respective species page in RD:
```{.r}
#single species:
herpSearch(binomial = "Apostolepis adhara")
```

**Function `herpAdvancedSearch`:**

Creates a link for a page as derived from an Advanced Search in RD (multiple species in a page):
```{.r}
#create multiple species link:
link <- herpAdvancedSearch(genus = "Apostolepis") #returns a link to access a list of all Apostolepis species
link <- herpAdvancedSearch(higher = "snakes", location = "Brazil") #returns a link to access a list of all snake species in Brazil
```

**Function `herpSpecies`:**

Sample species data from the species link created by `herpAdvancedSearch`. It includes higher taxa information, authors, year of description, and the species url.

- Returns higher taxa information and species url:
```{.r}
#sample multiple species data:
apo <- herpSpecies(link, taxonomicInfo = TRUE, getLink = TRUE) 
```
- Returns only species url - Faster and recommended for large datasets:
```{.r}
apo <- herpSpecies(link, taxonomicInfo = FALSE, getLink = TRUE)
```
- Returns species higher taxa from an user provided file with species url, divides sampling in 3 batches (recommended for large datasets):
```{.r}
apo <- herpSpecies(dataList = apo, taxonomicInfo = TRUE, getLink = FALSE, batches = 3)
```

**Function `herpSynonyms`:**

Samples species synonyms using a data frame with species names and the species link (e.g.: the result of `herpSpecies(link, getLink=TRUE)`).

```{.r}
#sample species synonyms
apo_syn <- herpSynonyms(apo)
```
**Function `herpSync`:**

Inspired in function aswSync from package [AmphiNom](https://github.com/hcliedtke/AmphiNom) (Liedtke, 2018), this function compares a given list of species (e.g.: IUCN, or a regional list) with the list of species synonyms and returns a tyde comparison allowing faster nomenclature check.

```{.r}
#comparing synonyms:
apo_list <- c("Vieira-Alencar authorisensis",
              "Apostolepis ambiniger",
              "Apostolepis cerradoensis",
              "Elapomorphus assimilis",
              "Apostolepis tertulianobeui",
              "Apostolepis goiasensis")

herpSync(apo_list, apo_syn)
```

### **Internal datasets**

- The package counts with a full list of current valid species (`allReptiles` - 12,440 species) with their respective higher taxa information (updated to 23th of May, 2025);

- A dataset with all unique synonyms for each current valid species (`allSynonyms` - 53,143 entries - updated to 19th of May, 2025 - necessary update coming soon); 

- Another synonyms dataset all entries considering their respective references (109,082 entries - updated to 19th of May, 2025 - necessary update coming soon).

### **Next steps**

- [ ] &nbsp; CRAN release
- [ ] &nbsp; Paper submission
- [ ] &nbsp; Implement adapted version of `AmphiNom::asw_stats()` (Liedtke, 2018)
- [ ] &nbsp; Implement adapted version of `AmphiNom::synonym_report()` (Liedtke, 2018)
- [ ] &nbsp; Implement IUCN match

### **References**
Liedtke, H. C. (2018). AmphiNom: an amphibian systematic tool. Systematics and Biodiversity, 17(1) 1-6. https://doi.org/10.1080/14772000.2018.1518935

Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025) The Reptile Database. http://www.reptile-database.org
