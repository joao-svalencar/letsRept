test_that("reptSplitCheck returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  query <- c("Vieira-Alencar authoristicus",
             "Tantilla melanocephala",
             "Oxybelis aeneus",
             "Apostolepis dimidiata",
             "Bothrops pauloensis")
  
  expect_warning(
  result <- reptSplitCheck(x=query, pubDate = 2019, cores = 1, showProgress = FALSE),
  "No species were found"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gte(nrow(result), 0)
  
})
