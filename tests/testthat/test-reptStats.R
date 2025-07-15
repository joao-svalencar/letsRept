test_that("reptCompare returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- reptStats(family="Boidae")
  
  expect_s3_class(result, "data.frame")
  expect_true("taxa" %in% names(result))
  expect_gt(nrow(result), 0)
})
