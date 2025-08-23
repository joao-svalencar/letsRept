test_that("reptCompare returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  my_species <- data.frame(species = c("Boa constrictor", "Pantherophis guttatus", "Fake species"))
  
  expect_warning(
  result <- reptCompare(my_species),
  "No RDB list provided"
  )
  expect_s3_class(result, "data.frame")
  expect_true("species" %in% names(result))
  expect_gte(nrow(result), 0)
})
