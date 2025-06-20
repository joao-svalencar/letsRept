test_that("herpSynonyms returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  boa <- letsHerp::allReptiles[grep("^Boa\\s", letsHerp::allReptiles$species),]
  result <- herpSynonyms(boa, getRef = FALSE, cores=2, showProgress = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true("synonyms" %in% names(result))
  expect_gt(nrow(result), 0)
})
