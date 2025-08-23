test_that("reptSplitCheck returns expected output", {
  skip_on_cran()
  skip_if_offline()
  
  query <- c("Tantilla melanocephala",
             "Oxybelis aeneus",
             "Apostolepis dimidiata",
             "Bothrops pauloensis")
  
  
  result <- reptSplitCheck(x=query, pubDate = 2019, cores = 1, showProgress = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gte(nrow(result), 0)
  
})
