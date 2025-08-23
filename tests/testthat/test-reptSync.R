test_that("reptSync returns expected output", {
  skip_on_cran()
  skip_if_offline()
  
  query <- c("Boa atlantica",
             "Boa diviniloqua",
             "Boa imperator")
  
  result <- reptSync(x=query, cores = 1, showProgress = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gte(nrow(result), 0)

})
