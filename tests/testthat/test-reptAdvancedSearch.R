test_that("reptAdvancedSearch returns expected output", {
  skip_on_cran()
  skip_if_not(Sys.getenv("NOT_CRAN") == "true")
  
  result <- reptAdvancedSearch(higher = "snakes", year = "2010", location = "Brazil")
  
  expect_type(result, "character")
  expect_true(grepl("https://reptile-database.reptarium.cz/advanced_search?", result))
})
