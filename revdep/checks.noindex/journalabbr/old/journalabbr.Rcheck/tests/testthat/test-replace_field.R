test_that("Determine data type, Function replace_field_journal()", {

  csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
  abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
  colnames(abbrtable_user)

  file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
  dt <- read_bib2dt(file)

  newdt <- replace_field_journal(dt, abbrtable_user)
  newdt1 <- replace_field_author(dt, author.connect = "and")
  newdt2 <- replace_field_author(dt, author.connect = "&")

  expect_true(is.data.table(newdt) && is.data.table(newdt1) &&  is.data.table(newdt2))
})
