test_that("expand completes all values", {
  df <- data.table(x = 1:2, y = 1:2)
  out <- expand(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("expand works with negative values, #282", {
  df <- data.table(x = 1:2)
  out <- expand(df, nesting(x), y = c(-1.0, -2.0))
  expect_equal(out$x, c(1, 1, 2, 2))
  expect_equal(out$y, c(-2, -1, -2, -1))
})

test_that("auto-converts data.frame inputs", {
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("preserves ordered factors", {
  df <- data.table(a = ordered("a"))
  out <- expand(df, a)
  expect_equal(df$a, ordered("a"))
})

test_that("preserves NAs", {
  x <- c(NA, "B", "A")
  expect_equal(crossing(x)$x, c("A", "B", NA))
  expect_equal(nesting(x)$x, c("A", "B", NA))
})

test_that("zero length input gives zero length output", {
  df <- tidytable(x = character())
  expect_equal(expand(df, x), df)

  expect_equal(
    expand_grid(x = integer(), y = 1:3),
    tidytable(x = integer(), y = integer())
  )
})

test_that("Works with .by", {
  test_df <- tidytable(id = c(1, 2), start = c(2, 3))

  result_df <- test_df %>%
    expand(start_end = start:3, start, .by = id)

  expect_named(result_df, c("id", "start_end", "start"))

  expect_equal(result_df$id, c(1,1,2))
  expect_equal(result_df$start_end, c(2,3,3))
  expect_equal(result_df$start, c(2,2,3))
})

test_that("Works on grouped_tt", {
  test_df <- tidytable(id = c(1, 2), start = c(2, 3))

  result_df <- test_df %>%
    group_by(id) %>%
    expand(start_end = start:3, start)

  expect_named(result_df, c("id", "start_end", "start"))

  expect_equal(result_df$id, c(1,1,2))
  expect_equal(result_df$start_end, c(2,3,3))
  expect_equal(result_df$start, c(2,2,3))
  expect_equal(group_vars(result_df), "id")
})

# nesting() ---------------------------------------------------
test_that("nesting works", {
  out <- nesting(x = c(2, 1, 1), y = c(2, 1, 1))
  expect_named(out, c("x", "y"))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, c(1, 2))
})

test_that("nesting works in expand", {
  df <- tidytable(x = c(1, 1, 2), y = c(1, 1, 2))
  out <- expand(df, nesting(x, y))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, c(1, 2))
})

test_that("nesting doesn't expand values", {
  df <- tidytable(x = 1:2, y = 1:2)
  expect_equal(expand(df, nesting(x, y)), df)
})

test_that("unnamed data frames are flattened", {
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand(df, nesting(x, y))
  expect_equal(out$x, df$x)

  out <- crossing(df)
  expect_equal(out$x, df$x)
})
