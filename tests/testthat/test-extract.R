test_that("extract.() removes old column and creates new one", {
  data.table(y = c(paste0(1:2, "-", 1:2), 1:4)) -> test_df
  test_df %>% extract.(y, "A", regex="([[:alnum:]]+)") -> results_df

  expect_equal(results_df$A, c("1", "2", "1", "2", "3", "4"))
  expect_null(results_df$y)
})

test_that("extract.() can extract 2 or more columns", {
  data.table(y = c(paste0(1:2, "-", 1:2), 1:4)) -> test_df
  test_df %>% extract.(y, c("A", "B"), regex="([[:alnum:]]+)-([[:alnum:]]+)") -> results_df

  expect_equal(results_df$A, c("1", "2", NA, NA, NA, NA))
  expect_equal(results_df$B, c("1", "2", NA, NA, NA, NA))
  expect_null(results_df$y)
})


test_that("extract.() error when the extracted groups number is different then the provided `into` columns", {
  data.table(y = c(paste0(1:2, "-", 1:2), 1:4)) -> test_df

  expect_error(test_df %>% extract.(y, c("A", "B"), regex="([[:alnum:]]+)"))
})