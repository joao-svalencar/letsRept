test_that("reptTidySyn runs without error", {
  df <- data.frame(
    col1 = c("Species1", "Species2"),
    col2 = c("syn1; syn2", "syn3"),
    stringsAsFactors = FALSE
  )
  expect_invisible(reptTidySyn(df))  # function prints, so invisibly returns NULL
})
