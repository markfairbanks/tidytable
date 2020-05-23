test_that("dt_ works without by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    dt_slice(1:4)

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:4)

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without by with .N", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:.N)

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without by with n.()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:n.())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without by with n.()", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  sliced_df <- test_df %>%
    slice.(1:n.())

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works without by with data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1:4)

  expect_equal(sliced_df, head(as_tidytable(test_df), 4))
})

test_that("works with by", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with by enhanced selection", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, by = where(is.character))

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with by w/ data.frame", {
  test_df <- data.frame(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"),
                        stringsAsFactors = FALSE)
  sliced_df <- test_df %>%
    slice.(1, by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works with by with data.frame", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    slice.(1, by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

test_that("works in custom function", {
  test_df <- tidytable(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))

  slice_fn <- function(.df, val) {
    .df %>%
      slice.(val, by = z)
  }

  sliced_df <- test_df %>%
    slice_fn(1)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})

# slice_head.() ----------------------------------------------------

test_that("dt_slice_head() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    dt_slice_head()

  expect_equal(sliced_df, head(test_df, 5))
})

test_that("_head.() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head.()

  expect_equal(sliced_df, head(test_df, 5))
})


test_that("_head.() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_head.(n = 3)

  expect_equal(sliced_df, head(test_df, 3))
})

test_that("_head.() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  datatable_df <- test_df[, head(.SD, 3), by = z]
  sliced_df <- test_df %>%
    dt_slice_head(n = 3, by = z)

  expect_equal(datatable_df, sliced_df)
})

# slice_tail.() ----------------------------------------------------

test_that("dt_slice_tail() works when empty", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    dt_slice_tail()

  expect_equal(sliced_df, tail(test_df, 5))
})

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

test_that("_tail.() works with n specified", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_tail.(n = n.())

  expect_equal(sliced_df, test_df)
})

test_that("_tail() works with n specified with by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))

  datatable_df <- test_df[, tail(.SD, 3), by = z]
  sliced_df <- test_df %>%
    slice_tail.(n = 3, by = z)

  expect_equal(datatable_df, sliced_df)
})

# slice_min.() ----------------------------------------------------

test_that("dt_slice_min() works", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    dt_slice_min(order_by = y, n = 3)

  expect_equal(sliced_df$x, c(10,9,8))
  expect_equal(sliced_df$y, c(11,12,13))
})

test_that("_min.() works", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = y, n = 3)

  expect_equal(sliced_df$x, c(10,9,8))
  expect_equal(sliced_df$y, c(11,12,13))
})

test_that("_min.() works with by", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = x, n = 3, by = z)

  expect_equal(sliced_df$z, c("a", "a", "a", "b", "b", "b"))
  expect_equal(sliced_df$y, c(20,19,18,14,13,12))
})

test_that("_min.() works with by enhanced selection", {
  test_df <- tidytable(x = 1:10, y = 20:11, z = c(rep("a", 6), rep("b", 4)))
  sliced_df <- test_df %>%
    slice_min.(order_by = x, n = 3, by = where(is.character))

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
