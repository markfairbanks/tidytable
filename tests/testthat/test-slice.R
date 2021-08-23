test_that("works without .by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("doesn't return NAs", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:5)

  expect_equal(sliced_df, test_df)
})

test_that("works with gaps & doesn't return NAs", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(c(1,3,5))

  expect_equal(sliced_df$x, c(1,3))
})

test_that("can slice when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

test_that("can use dots", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1,3,5)

  expect_equal(sliced_df$x, c(1,3))
})

test_that("works without .by with .N", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(2:.N)

  comp_df <- test_df %>%
    head(4) %>%
    tail(3)

  expect_equal(sliced_df, comp_df)
})

test_that("works without .by with n.()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:n.())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without .by with n.()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:n.())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without .by with data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1:4)

  expect_equal(sliced_df, head(as_tidytable(test_df), 4))
})

test_that("negative numbers drop rows", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(-3, -4)

  expect_equal(sliced_df, head(as_tidytable(test_df), 2))
})

test_that("works with .by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by enhanced selection", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, .by = where(is.character))

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by w/ data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"),
                        stringsAsFactors = FALSE)
  sliced_df <- test_df %>%
    slice.(1, .by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with .by with data.frame", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, .by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works in custom function", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  slice_fn <- function(.df, val) {
    .df %>%
      slice.(val, .by = z)
  }

  sliced_df <- test_df %>%
    slice_fn(1)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("preserves column order when .by is used", {
  test_df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  sliced_df <- slice.(test_df, 1, .by = y)

  expect_named(test_df, c("x", "y"))
})

# slice_head.() ----------------------------------------------------

test_that("_head.() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head.()

  expect_equal(sliced_df, head(test_df, 5))
})

test_that("_head.() works when empty, doesn't return NAs", {
  test_df <- tidytable(x = 1:3, y = 1:3)
  sliced_df <- test_df %>%
    slice_head.()

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("_head.() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head.(n = 3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("_head.() works with n specified with by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  col_order <- names(test_df)

  datatable_df <- test_df[, head(.SD, 3), by = z][, ..col_order]
  sliced_df <- test_df %>%
    slice_head.(n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(datatable_df, sliced_df)
})

test_that("_head.() works in custom function", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  slice_head_fn <- function(.df, val) {
    .df %>%
      slice_head.(val)
  }

  sliced_df <- test_df %>%
    slice_head_fn(3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("can slice_head when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice_head.(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

# slice_tail.() ----------------------------------------------------

test_that("_tail.() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail.()

  expect_equal(sliced_df, tail(test_df, 5))
})


test_that("_tail.() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail.(n = 3)

  expect_equal(sliced_df, tail(test_df, 3))
})

test_that("_tail.() works with n.()", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail.(n = n.())

  expect_equal(sliced_df, test_df)
})

test_that("_tail() works with n specified with .by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  col_order <- names(test_df)

  datatable_df <- test_df[, tail(.SD, 3), by = z][, ..col_order]
  sliced_df <- test_df %>%
    slice_tail.(n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(datatable_df, sliced_df)
})

test_that("_tail.() works with a custom function", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  slice_tail_fn <- function(.df, val) {
    .df %>%
      slice_tail.(val)
  }

  sliced_df <- test_df %>%
    slice_tail_fn(3)

  expect_equal(sliced_df, tail(test_df, 3))
})

test_that("can slice_tail when all cols are in .by", {
  test_df <- tidytable(x = c("a", "a", "b"))

  sliced_df <- test_df %>%
    slice_tail.(1, .by = x)

  expect_named(sliced_df, c("x"))
  expect_equal(sliced_df$x, c("a", "b"))
})

# slice_min.() ----------------------------------------------------

test_that("_min.() works", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = y, n = 3)

  expect_equal(sliced_df$x, c(10,9,8))
  expect_equal(sliced_df$y, c(11,12,13))
})

test_that("_min.() works with .by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = x, n = 3, .by = z)

  expect_named(sliced_df, names(test_df))
  expect_equal(sliced_df$z, c("a", "a", "a", "b", "b", "b"))
  expect_equal(sliced_df$y, c(20,19,18,14,13,12))
})

test_that("_min.() works with .by enhanced selection", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = x, n = 3, .by = where(is.character))

  expect_equal(sliced_df$z, c("a", "a", "a", "b", "b", "b"))
  expect_equal(sliced_df$y, c(20,19,18,14,13,12))
})

test_that("_max.() works with custom function with quosures", {
  test_df <- data.table(a = 1:3, b = 4:6)

  slice_fn <- function(.df, col, num) {
    .df %>%
      slice_max.({{ col }}, num)
  }

  sliced_df <- test_df %>%
    slice_fn(a, 1)

  expect_equal(sliced_df$a, 3)
  expect_equal(sliced_df$b, 6)
})
