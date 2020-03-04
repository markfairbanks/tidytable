test_that("can do group aggregation with by", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    dt_summarize(avg_x = mean(x), by = y)

  datatable_df <- df[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation with by w/ data.frame", {
  df <- data.frame(x = 1:4, y = c("a","a","a","b"),
                   stringsAsFactors = FALSE)

  tidytable_df <- df %>%
    dt_summarize(avg_x = mean(x), by = y)

  datatable_df <- as_tidytable(df)[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation without by with data.frame", {
  df <- data.frame(x = 1:4, y = c("a","a","a","b"),
                   stringsAsFactors = FALSE)

  tidytable_df <- df %>%
    dt_summarize(avg_x = mean(x))

  datatable_df <- as_tidytable(df)[, list(avg_x = mean(x))]

  expect_equal(tidytable_df, datatable_df)
})
