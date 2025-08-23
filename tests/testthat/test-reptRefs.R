test_that("reptRefs returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- reptRefs("Boa constrictor")
  
  expect_s3_class(result, "data.frame")
  expect_true("link" %in% names(result))
  expect_gte(nrow(result), 0)
})
