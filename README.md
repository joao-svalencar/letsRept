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

To access the full list of functions with clear examples, browse the package vignettes:

```{.r}
browseVignettes("letsRept")
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
- [x] &nbsp; `reptSync` upgrade
- [x] &nbsp; Implement "up_to_date" check (`reptSplitCheck`)
- [x] &nbsp; Implement Reference sampling (df with links)
- [x] &nbsp; Implement adapted version of `AmphiNom::asw_stats()` (Liedtke, 2018)
- [ ] $nbsp; Stable version 1.0.0 CRAN submission (August 18th, 2025)
- [ ] $nbsp; Stable version 1.0.0 CRAN release
- [ ] &nbsp; Paper submission (ongoing)

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
