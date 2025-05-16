# Generating internal database (HerpNom) ----------------------------------

allReptiles <- HerpNom::getSpecies("https://reptile-database.reptarium.cz/advanced_search?location=-hogwarts&submit=Search")

allSynonyms <- HerpNom::getSynonyms(HerpNom::allReptiles[1:500,])
allSynonyms2 <- HerpNom::getSynonyms(HerpNom::allReptiles[501:1000,])
allSynonyms3 <- HerpNom::getSynonyms(HerpNom::allReptiles[1001:1500,])
allSynonyms4 <- HerpNom::getSynonyms(HerpNom::allReptiles[1501:2000,])
allSynonyms5 <- HerpNom::getSynonyms(HerpNom::allReptiles[2001:2500,])
allSynonyms6 <- HerpNom::getSynonyms(HerpNom::allReptiles[2501:3000,])
allSynonyms7 <- HerpNom::getSynonyms(HerpNom::allReptiles[3001:3500,])
allSynonyms7 <- HerpNom::getSynonyms(HerpNom::allReptiles[3501:4000,])
allSynonyms8 <- HerpNom::getSynonyms(HerpNom::allReptiles[4001:4500,])
allSynonyms9 <- HerpNom::getSynonyms(HerpNom::allReptiles[4501:5000,])
allSynonyms10 <- HerpNom::getSynonyms(HerpNom::allReptiles[5001:5500,])
allSynonyms11 <- HerpNom::getSynonyms(HerpNom::allReptiles[5501:6000,])
allSynonyms12 <- HerpNom::getSynonyms(HerpNom::allReptiles[6001:6500,])
allSynonyms13 <- HerpNom::getSynonyms(HerpNom::allReptiles[6501:7000,])
allSynonyms14 <- HerpNom::getSynonyms(HerpNom::allReptiles[7001:7500,])
allSynonyms15 <- HerpNom::getSynonyms(HerpNom::allReptiles[7501:8000,])
allSynonyms16 <- HerpNom::getSynonyms(HerpNom::allReptiles[8001:8500,])
allSynonyms17 <- HerpNom::getSynonyms(HerpNom::allReptiles[8501:9000,])
allSynonyms18 <- HerpNom::getSynonyms(HerpNom::allReptiles[9001:9500,])
allSynonyms19 <- HerpNom::getSynonyms(HerpNom::allReptiles[9501:10000,])
allSynonyms20 <- HerpNom::getSynonyms(HerpNom::allReptiles[10001:10500,])
allSynonyms21 <- HerpNom::getSynonyms(HerpNom::allReptiles[10501:11000,])
allSynonyms22 <- HerpNom::getSynonyms(HerpNom::allReptiles[11001:11500,])
allSynonyms23 <- HerpNom::getSynonyms(HerpNom::allReptiles[11501:12000,])
allSynonyms24 <- HerpNom::getSynonyms(HerpNom::allReptiles[12001:12384,])

usethis::use_data(allReptiles, overwrite = TRUE)
usethis::use_data(allSynonyms, overwrite = TRUE)

devtools::document()

devtools::build()
devtools::check()


cat(paste("Species number",paste0(i,"",":"), "\n", HerpNom::allReptiles$species[i],"\n", "Done!"))


