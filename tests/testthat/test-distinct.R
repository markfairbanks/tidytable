test_that("dt_distinct() works on all rows", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"))
  distinct_df <- test_df %>%
    dt_distinct()

  expect_equal(names(test_df), names(distinct_df))
  expect_equal(test_df, distinct_df)
})

test_that("dt_distinct() works on 1 column", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    dt_distinct(b)

  expect_named(distinct_df, c("b"))
  expect_equal(test_df$b, 4:6)
})

test_that("dt_distinct() works with enhanced selection", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    dt_distinct(is.character)

  expect_named(distinct_df, c("c", "d"))
  expect_equal(distinct_df$c, c("a", "b"))
  expect_equal(distinct_df$d, c("a", "b"))
})
