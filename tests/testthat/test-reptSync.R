test_that("reptSync returns expected output", {
  skip_on_cran()
  skip_if_offline()
  
  query <- c("Vieira-Alencar authoristicus",
             "Boa atlantica",
             "Boa diviniloqua",
             "Boa imperator")
  
  expect_warning(
  result <- reptSync(x=query, cores = 1, showProgress = FALSE),
  "No species were found"
  )

  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gte(nrow(result), 0)

})
