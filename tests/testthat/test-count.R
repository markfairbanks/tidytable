test_that("empty count.() returns number of rows", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  summary_df <- test_df %>%
    count.()

  expect_named(summary_df, c("N"))
  expect_equal(summary_df$N, nrow(test_df))
})

test_that("count.() works on data.frame", {
  test_df <- data.frame(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  summary_df <- test_df %>%
    count.()

  expect_named(summary_df, c("N"))
  expect_equal(summary_df$N, nrow(test_df))
})

test_that("dt_count(val) returns group results", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  summary_df <- test_df %>%
    count.(d)

  expect_named(summary_df, c("d", "N"))
  expect_equal(summary_df$d, c("a", "b"))
  expect_equal(summary_df$N, c(2, 1))
})

test_that("count.() works with enhanced selection", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  summary_df <- test_df %>%
    count.(where(is.character))

  expect_named(summary_df, c("c", "d", "N"))
  expect_equal(summary_df$c, c("a", "a"))
  expect_equal(summary_df$d, c("a", "b"))
  expect_equal(summary_df$N, c(2, 1))
})

test_that("can make a function with quosures", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))

  count_fn <- function(.df, col) {
    count.(.df, {{col}})
  }

  summary_df <- test_df %>%
    count_fn(d)

  expect_named(summary_df, c("d", "N"))
  expect_equal(summary_df$d, c("a", "b"))
  expect_equal(summary_df$N, c(2, 1))
})
