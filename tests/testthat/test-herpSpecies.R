test_that("herpSpecies returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- herpSpecies(herpAdvancedSearch(genus = "Boa"), taxonomicInfo = TRUE, cores = 2)
  
  expect_s3_class(result, "data.frame")
  expect_true("species" %in% names(result))
  expect_gt(nrow(result), 0)
})
