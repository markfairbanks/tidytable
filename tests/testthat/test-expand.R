test_that("expand completes all values", {
  df <- data.table(x = 1:2, y = 1:2)
  out <- expand.(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("expand works with negative values, #282", {
  df <- data.table(x = 1:2)
  out <- expand.(df, nesting.(x), y = c(-1.0, -2.0))
  expect_equal(out$x, c(1, 1, 2, 2))
  expect_equal(out$y, c(-2, -1, -2, -1))
})

test_that("auto-converts data.frame inputs", {
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand.(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("crossing works with naming", {
  x <- 1:2
  y <- 1:2
  out <- crossing.(x, new_y = y)
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("expand_grid works with naming", {
  x <- 1:2
  y <- 1:2
  out <- expand_grid.(x, new_y = y)
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("expand_grid works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- expand_grid.(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("crossing works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- crossing.(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("expand_grid works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- expand_grid.(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("crossing works when only input is a data frame", {
  test_df <- tidytable(a = c("a", "a", "b"), b = c("a", "a", "b"))
  crossing_df <- crossing.(test_df)
  unique_df <- unique(test_df)

  expect_equal(crossing_df, unique_df)
})

test_that("crossing works with data.frame inputs", {
  test_df <- tidytable(a = c("a", "b"), b = c("a", "b"))
  crossing_df <- crossing.(test_df, x = 1:2)

  expect_named(crossing_df, c("a", "b", "x"))
  expect_equal(crossing_df$a, c("a", "a", "b", "b"))
  expect_equal(crossing_df$b, c("a", "a", "b", "b"))
  expect_equal(crossing_df$x, rep(1:2, 2))
})

test_that("expand_grid works with data.frame inputs", {
  test_df <- tidytable(a = c("b", "a"), b = c("b", "a"))
  expand_df <- expand_grid.(test_df, x = 1:2)

  expect_named(expand_df, c("a", "b", "x"))
  expect_equal(expand_df$a, c("b", "b", "a", "a"))
  expect_equal(expand_df$b, c("b", "b", "a", "a"))
  expect_equal(expand_df$x, rep(1:2, 2))
})

test_that("preserves ordered factors", {
  df <- data.table(a = ordered("a"))
  out <- expand.(df, a)
  expect_equal(df$a, ordered("a"))
})

test_that("preserves NAs", {
  x <- c(NA, "A", "B")
  expect_equal(crossing.(x)$x, x)
  expect_equal(nesting.(x)$x, x)
})

test_that("zero length input gives zero length output", {
  df <- tidytable(x = character())
  expect_equal(expand.(df, x), df)

  expect_equal(
    expand_grid.(x = integer(), y = 1:3),
    tidytable(x = integer(), y = integer())
  )
})

test_that("Works with .by", {
  test_df <- tidytable(id = c(1, 2), start = c(2, 3))

  result_df <- test_df %>%
    expand.(start_end = start:3, start, .by = id)

  expect_named(result_df, c("id", "start_end", "start"))

  expect_equal(result_df$id, c(1,1,2))
  expect_equal(result_df$start_end, c(2,3,3))
  expect_equal(result_df$start, c(2,2,3))
})

# nesting() ---------------------------------------------------
test_that("nesting works", {
  df <- tidytable(x = c(1, 1, 2), y = c(1, 1, 2))
  out <- expand.(df, nesting.(x, y))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, c(1, 2))
})

test_that("nesting doesn't expand values", {
  df <- tidytable(x = 1:2, y = 1:2)
  expect_equal(expand.(df, nesting.(x, y)), df)
})

test_that("unnamed data frames are flattened", {
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand.(df, nesting.(x, y))
  expect_equal(out$x, df$x)

  out <- crossing.(df)
  expect_equal(out$x, df$x)
})
