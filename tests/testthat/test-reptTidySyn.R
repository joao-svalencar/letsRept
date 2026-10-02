test_that("reptTidySyn runs without error", {
  df <- data.frame(
    query = c("Species1", "Species2"),
    RDB = c("syn1; syn2", "syn3"),
    status = c("updated", "up_to_date"),
    stringsAsFactors = FALSE
  )
  expect_invisible(reptTidySyn(df))  # function prints, so invisibly returns NULL
})
