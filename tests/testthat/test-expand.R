test_that("expand completes all values", {
  df <- data.table(x = 1:2, y = 1:2)
  out <- expand.(df, x, y)
  expect_equal(nrow(out), 4)
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

test_that("crossing work with data.frame inputs", {
  test_df <- tidytable(a = c("a", "a", "b"), b = c("a", "a", "b"))
  crossing_df <- crossing.(test_df)
  unique_df <- unique(test_df)

  expect_equal(crossing_df, unique_df)
})

test_that("crossing works when only input is a data frame", {
  test_df <- tidytable(a = c("a", "b"), b = c("a", "b"))
  crossing_df <- crossing.(test_df, x = 1:2)

  expect_named(crossing_df, c("a", "b", "x"))
  expect_equal(crossing_df$a, c("a", "a", "b", "b"))
  expect_equal(crossing_df$b, c("a", "a", "b", "b"))
  expect_equal(crossing_df$x, rep(1:2, 2))
})

test_that("preserves ordered factors", {
  df <- data.table(a = ordered("a"))
  out <- expand.(df, a)
  expect_equal(df$a, ordered("a"))
})

test_that("preserves NAs", {
  x <- c(NA, "A", "B")
  expect_equal(crossing.(x)$x, x)
  # expect_equal(nesting.(x)$x, x)
})

# test_that("NULL inputs", {
#   tb <- tidytable(x = 1:5)
#   expect_equal(expand.(tb, x, y = NULL), tb)
#   # expect_equal(nesting(x = tb$x, y = NULL), tb)
#   expect_equal(crossing.(x = tb$x, y = NULL), tb)
# })

test_that("zero length input gives zero length output", {
  tb <- tidytable(x = character())
  expect_equal(expand.(tb, x), tb)

  expect_equal(
    expand_grid.(x = integer(), y = 1:3),
    tidytable(x = integer(), y = integer())
  )
})
