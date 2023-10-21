# slice_head() ----------------------------------------------------

test_that("_head() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head()

  expect_equal(sliced_df, head(test_df, 5))
})

test_that("_head() works when empty, doesn't return NAs", {
  test_df <- tidytable(x = 1:3, y = 1:3)
  sliced_df <- test_df %>%
    slice_head()

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("_head() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head(n = 3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("_head() works with n specified with .by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  col_order <- names(test_df)

  datatable_df <- test_df[, head(.SD, 3), by = z][, ..col_order]
  sliced_df <- test_df %>%
    slice_head(n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(datatable_df, sliced_df)

  sliced_df <- test_df %>%
    slice_head(n = 3, by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(datatable_df, sliced_df)
})

test_that("_head() works in custom function", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  slice_head_fn <- function(.df, val) {
    .df %>%
      slice_head(val)
  }

  sliced_df <- test_df %>%
    slice_head_fn(3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("can slice_head when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice_head(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

test_that("_head works on 0-row data frame, #642", {
  df <- tidytable(x = integer(), y = character())
  res <- slice_head(df, 2)
  expect_equal(df, res)
})

test_that("works with grouped_tt", {
  test_df <- tidytable(x = c("a", "a", "b"), y = 1:3)

  sliced_df <- test_df %>%
    group_by(x) %>%
    slice_head(1)

  expect_named(sliced_df, c("x", "y"))
  expect_equal(sliced_df$y, c(1, 3))
  expect_equal(group_vars(sliced_df), "x")
  expect_true(is_grouped_df(sliced_df))
})

# slice_tail() ----------------------------------------------------

test_that("_tail() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail()

  expect_equal(sliced_df, tail(test_df, 5))
})

test_that("_tail() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail(n = 3)

  expect_equal(sliced_df, tail(test_df, 3))
})

test_that("_tail() works with n()", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail(n = n())

  expect_equal(sliced_df, test_df)
})

test_that("_tail() works with n specified with .by/by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  col_order <- names(test_df)

  datatable_df <- test_df[, tail(.SD, 3), by = z][, ..col_order]
  sliced_df <- test_df %>%
    slice_tail(n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(datatable_df, sliced_df)

  sliced_df <- test_df %>%
    slice_tail(n = 3, by = z)

  expect_equal(datatable_df, sliced_df)
})


test_that("_tail() works with a custom function", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  slice_tail_fn <- function(.df, val) {
    .df %>%
      slice_tail(val)
  }

  sliced_df <- test_df %>%
    slice_tail_fn(3)

  expect_equal(sliced_df, tail(test_df, 3))
})

test_that("can slice_tail when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice_tail(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

test_that("_tail works on 0-row data frame, #642", {
  df <- tidytable(x = integer(), y = character())
  res <- slice_tail(df, 2)
  expect_equal(df, res)
})

test_that("works with grouped_tt", {
  test_df <- tidytable(x = c("a", "a", "b"), y = 1:3)

  sliced_df <- test_df %>%
    group_by(x) %>%
    slice_tail(1)

  expect_named(sliced_df, c("x", "y"))
  expect_equal(sliced_df$y, c(2, 3))
  expect_equal(group_vars(sliced_df), "x")
  expect_true(is_grouped_df(sliced_df))
})
