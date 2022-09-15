test_that("works without .by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1:3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("slice.", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("doesn't return NAs", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1:5)

  expect_equal(sliced_df, test_df)
})

test_that("works with gaps & doesn't return NAs", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(c(1,3,5))

  expect_equal(sliced_df$x, c(1,3))
})

test_that("can slice when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

test_that("can use dots", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1,3,5)

  expect_equal(sliced_df$x, c(1,3))
})

test_that("works without .by with .N", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(2:.N)

  comp_df <- test_df %>%
    head(4) %>%
    tail(3)

  expect_equal(sliced_df, comp_df)
})

test_that("works without .by with n()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1:n())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without .by with n()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice(1:n())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without .by with data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice(1:4)

  expect_equal(sliced_df, head(as_tidytable(test_df), 4))
})

test_that("negative numbers drop rows", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice(-3, -4)

  expect_equal(sliced_df, head(as_tidytable(test_df), 2))
})

test_that("works with .by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice(1, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by enhanced selection", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice(1, .by = where(is.character))

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by w/ data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"),
                        stringsAsFactors = FALSE)
  sliced_df <- test_df %>%
    slice(1, .by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by with data.frame", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice(1, .by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works in custom function", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  slice_fn <- function(.df, val) {
    .df %>%
      slice(val, .by = z)
  }

  sliced_df <- test_df %>%
    slice_fn(1)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("preserves column order when .by is used", {
  test_df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  sliced_df <- slice(test_df, 1, .by = y)

  expect_named(test_df, c("x", "y"))
})

test_that("works on grouped_tt", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    group_by(z) %>%
    slice(1)

  expect_named(sliced_df, names(test_df))
  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
  expect_equal(group_vars(sliced_df), "z")
  expect_true(is_grouped_df(sliced_df))
})

