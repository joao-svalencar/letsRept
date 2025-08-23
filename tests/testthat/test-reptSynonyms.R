test_that("reptSynonyms returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  boa <- letsRept::allReptiles[grep("^Boa\\s", letsRept::allReptiles$species),]
  
  result <- reptSynonyms(boa, getRef = FALSE, cores=1, showProgress = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true("synonyms" %in% names(result))
  expect_gte(nrow(result), 0)
})
