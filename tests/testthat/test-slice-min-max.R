test_that("_min() works", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min(order_by = y, n = 3)

  expect_equal(sliced_df$x, c(10,9,8))
  expect_equal(sliced_df$y, c(11,12,13))
})

test_that("_min() works with .by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min(order_by = x, n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(sliced_df$z, c("a", "a", "a", "b", "b", "b"))
  expect_equal(sliced_df$y, c(20,19,18,14,13,12))
})

test_that("_min() works with .by enhanced selection", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min(order_by = x, n = 3, .by = where(is.character))

  expect_equal(sliced_df$z, c("a", "a", "a", "b", "b", "b"))
  expect_equal(sliced_df$y, c(20,19,18,14,13,12))
})

test_that("_max() works with custom function with quosures", {
  test_df <- data.table(a = 1:3, b = 4:6)

  slice_fn <- function(.df, col, num) {
    .df %>%
      slice_max({{ col }}, num)
  }

  sliced_df <- test_df %>%
    slice_fn(a, 1)

  expect_equal(sliced_df$a, 3)
  expect_equal(sliced_df$b, 6)
})

test_that("min and max return ties by default", {
  df <- tidytable(x = c(1, 2, 1, 2, 1))
  expect_equal(df %>% slice_min(x) %>% pull(), c(1, 1, 1))
  expect_equal(df %>% slice_max(x) %>% pull(), c(2, 2))

  expect_equal(df %>% slice_min(x, with_ties = FALSE) %>% pull(), 1)
  expect_equal(df %>% slice_max(x, with_ties = FALSE) %>% pull(), 2)
})

test_that("_max works with grouped_tt", {
  test_df <- tidytable(x = c("a", "a", "b"), y = 1:3)

  sliced_df <- test_df %>%
    group_by(x) %>%
    slice_max(order_by = y)

  expect_named(sliced_df, c("x", "y"))
  expect_equal(sliced_df$y, c(3, 2))
  expect_equal(group_vars(sliced_df), "x")
  expect_true(is_grouped_df(sliced_df))
})

test_that("_min works with grouped_tt", {
  test_df <- tidytable(x = c("a", "a", "b"), y = c(2, 1, 3))

  sliced_df <- test_df %>%
    group_by(x) %>%
    slice_min(order_by = y)

  expect_named(sliced_df, c("x", "y"))
  expect_equal(sliced_df$y, c(1, 3))
  expect_equal(group_vars(sliced_df), "x")
  expect_true(is_grouped_df(sliced_df))
})
