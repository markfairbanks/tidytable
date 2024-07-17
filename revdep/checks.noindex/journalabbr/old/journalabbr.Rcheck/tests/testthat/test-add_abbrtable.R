test_that("Determine data type, Function add_abbrtable()", {
  csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
  abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
  colnames(abbrtable_user)
  expect_true(is.data.table(abbrtable_user))
})
