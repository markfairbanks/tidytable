test_that("can do group aggregation with .by", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x), .by = y)

  datatable_df <- df[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("n.() works", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(count = n.(), .by = y)

  datatable_df <- df[, list(count = .N), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("n.() works in 2nd spot", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x),
               count = n.(),
               .by = y)

  datatable_df <- df[, list(avg_x = mean(x), count = .N), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that(".GRP works", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(count = .GRP, .by = y)

  datatable_df <- df[, list(count = .GRP), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation with no by", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x))

  datatable_df <- df[, list(avg_x = mean(x))]

  expect_equal(tidytable_df, datatable_df)
})

test_that("by = list() causes an error", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  expect_error(summarize.(df, avg_x = mean(x), .by = list(y)))
})

test_that("by = list works for column named list", {
  df <- tidytable(x = 1:4, list = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x), .by = list)

  datatable_df <- df[, list(avg_x = mean(x)), by = list]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation with by c()", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x), .by = c(y))

  datatable_df <- df[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can use .sort & doesn't modify-by-reference", {
  test_df <- tidytable(x = 1:3, y = c("b", "a", "a"), z = c("b", "a", "a"))

  tidytable_df <- test_df %>%
    summarize.(avg_x = mean(x),
               .by = c(y, z),
               .sort = TRUE)

  datatable_df <- test_df[, list(avg_x = mean(x)), keyby = list(y, z)]

  expect_equal(tidytable_df$avg_x, datatable_df$avg_x)
  expect_named(test_df, c("x", "y", "z"))
})

test_that("can do group aggregation with by enhanced selection", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x), .by = where(is.character))

  datatable_df <- df[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation with by w/ data.frame", {
  df <- data.frame(x = 1:4, y = c("a","a","a","b"),
                   stringsAsFactors = FALSE)

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x), .by = y)

  datatable_df <- as_tidytable(df)[, list(avg_x = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can do group aggregation without by with data.frame", {
  df <- data.frame(x = 1:4, y = c("a","a","a","b"),
                   stringsAsFactors = FALSE)

  tidytable_df <- df %>%
    summarize.(avg_x = mean(x))

  datatable_df <- as_tidytable(df)[, list(avg_x = mean(x))]

  expect_equal(tidytable_df, datatable_df)
})

test_that("can make a function with quosures", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  summarize_fn <- function(.df, col, val) {
    .df %>%
      summarize.(avg_x = mean({{col}}) + val, .by = y)
  }

  tidytable_df <- df %>%
    summarize_fn(x, 1)

  datatable_df <- df[, list(avg_x = mean(x) + 1), by = y]

  expect_equal(tidytable_df, datatable_df)
})
