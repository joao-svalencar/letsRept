test_that("reptSearch returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- reptSearch("Philodryas livida", verbose = FALSE)
  
  expect_type(result, "list")
  expect_true("species" %in% names(result))
  expect_gt(length(result), 0)
})
