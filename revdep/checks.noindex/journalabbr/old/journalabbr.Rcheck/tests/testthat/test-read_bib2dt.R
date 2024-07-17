test_that("multiplication works, Function read_bib2dt()", {

  # Read from .bib file:
  file1 <- system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
  dt1 <- read_bib2dt(file1)
  colnames(dt1)

  file2 <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
  dt2 <- read_bib2dt(file2)
  colnames(dt2)


  expect_true(is.data.table(dt1))
  expect_true(is.data.table(dt2))
  expect_warning(read_bib2dt(file1),"Duplicate key in uploaded Bib file")
  expect_warning(read_bib2dt(file1),"NA value exists in Citation Key, please check the bib file")
  expect_warning(read_bib2dt(file2),"Duplicate key in uploaded Bib file")
})
