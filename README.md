# *letsRept*🦎 🐍 🐊 🐢

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/letsRept)](https://CRAN.R-project.org/package=letsRept)
[![pkgdown](https://img.shields.io/badge/pkgdown-letsRept-blue)](https://joao-svalencar.github.io/letsRept/)
<!-- badges: end -->
  
### **An Interface to the Reptile Database**

This package was developed to facilitate the processes of reptile nomenclature update based on the [Reptile Database](https://reptile-database.reptarium.cz) website (Uetz et al., 2025).

Currently, the package access many species information from the Reptile Database using R interface.

I hope it to be useful, to people trying to match databases from different sources (IUCN, species traits database, etc), or trying to get summaries from a given higher taxa or region (e.g.: Snakes from Brazil). But it can also just print single species information directly in R.

Any feedback, suggestion or request are welcome!

### **Download**

To install the development version run:

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

- The package counts with a full list of current valid species (`allReptiles` - 12,500 species) with their respective higher taxa information (updated to 15th of September, 2025);

- A dataset with all unique synonyms for each current valid species (`allSynonyms` - 53,159 entries - updated to 23th of May, 2025); 

- Another synonyms dataset with all entries considering their respective references (`allSynonymsRef`110,413 entries - updated to 23rd of May, 2025).

### **Next steps**

- [x] &nbsp; Stable version 1.0.1 CRAN submission (August 18th, 2025)
- [x] &nbsp; Stable version 1.0.1 CRAN release (August 22nd, 2025)
- [x] &nbsp; Paper submission (August 18th, 2025)
- [x] &nbsp; Paper publication (October 20th, 2025)
- [x] &nbsp; update `allReptiles`
- [ ] &nbsp; update `allSynonyms`
- [ ] &nbsp; Package webpage development
- [ ] &nbsp; API development and package API compatibility

### **How to Cite**

To cite this package in publications, run:

```r
citation("letsRept")
```

⚠️ Important note:

`letsRept` retrieves valuable taxonomic and synonymy data directly from the [Reptile Database](http://www.reptile-database.org).
When citing this package, please also cite the original database as a data source.

### **References**
Uetz, P., Freed, P, Aguilar, R., Reyes, F., Kudera, J. & Hošek, J. (eds.) (2025). The Reptile Database. http://www.reptile-database.org

Vieira-Alencar, J.P.S., Liedtke, H.C., Meire, S., Roll, U., Uetz, P. & Nori, J. (2025). letsRept: An R package to access the Global Reptile Database and facilitate taxonomic harmonization.  Biodiversity Informatics, 19, 120-143. https://doi.org/10.17161/bi.v19i.24329

### **Author:**

Vieira-Alencar, João Paulo dos Santos (joaopaulo.valencar@gmail.com)

[Orcid](https://orcid.org/0000-0001-6894-6773) | [Research Gate](https://www.researchgate.net/profile/Joao-Paulo-Alencar)

Ph.D in Ecology at USP

Post-Doc at:
Laboratório de Evolução e Diversidade I  - "LED 1" - Centro de Ciências Naturais e Humanas, Universidade Federal do ABC, São Bernardo do Campo, SP – Brazil
