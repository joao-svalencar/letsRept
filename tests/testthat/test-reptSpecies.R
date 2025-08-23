test_that("reptSpecies returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- reptSpecies(reptAdvancedSearch(genus = "Boa"),
                        taxonomicInfo = FALSE, cores = 1,
                        getLink = TRUE, showProgress = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true("species" %in% names(result))
  expect_gte(nrow(result), 0)
})
