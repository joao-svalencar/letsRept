test_that("reptSplitCheck returns expected output", {
  
  query <- c("Vieira-Alencar authoristicus",
             "Tantilla melanocephala",
             "Oxybelis aeneus",
             "Apostolepis dimidiata",
             "Bothrops pauloensis")
  
  result <- reptSplitCheck(x=query, pubDate = 2019, cores = 2, showProgress = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gt(nrow(result), 0)
})