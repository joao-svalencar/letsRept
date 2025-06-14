test_that("herpSync returns expected output", {
  boa_syn <- letsHerp::allSynonyms[grep("^Boa\\s", letsHerp::allSynonyms$species),]
  boa_list <- c("Vieira-Alencar authoristicus", "Boa atlantica", "Boa diviniloqua", "Boa imperator")
  result <- herpSync(boa_list, boa_syn)
  
  expect_s3_class(result, "data.frame")
  expect_true("Reptile_Database" %in% names(result))
  expect_gt(nrow(result), 0)
})
