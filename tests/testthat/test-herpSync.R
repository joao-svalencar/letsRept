test_that("herpSync returns expected output", {
  query <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
  
  result <- herpSync(x=query, cores = 2)

  expect_s3_class(result, "data.frame")
  expect_true("RDB" %in% names(result))
  expect_gt(nrow(result), 0)
})
