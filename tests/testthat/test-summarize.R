test_that("can do group aggregation with by", {
  df <- data.table(x = 1:4, y = c("a","a","a","b"))

  tidyfast_df <- df %>%
    dt_summarize(avg_x = mean(x), by = y)

  datatable_df <- df[, list(avg_x = mean(x)), by = y]

  expect_equal(tidyfast_df, datatable_df)
})
