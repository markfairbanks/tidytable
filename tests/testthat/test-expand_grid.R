test_that("crossing works with naming", {
  x <- 1:2
  y <- 1:2
  out <- crossing(x, new_y = y)
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("crossing. works", {
  x <- 1:2
  y <- 1:2
  out <- crossing.(x, new_y = y) %>%
    suppressWarnings()
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("expand_grid works with naming", {
  x <- 1:2
  y <- 1:2
  out <- expand_grid(x, new_y = y)
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("expand_grid works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- expand_grid(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("expand_grid. works", {
  x <- 1:2
  y <- 1:2
  out <- expand_grid.(x, new_y = y) %>%
    suppressWarnings()
  expect_equal(out$x, c(1,1,2,2))
  expect_equal(out$new_y, c(1,2,1,2))
})

test_that("crossing works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- crossing(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("expand_grid works with splicing", {
  test_list <- list(x = 1:2, y = 1:2)
  out <- expand_grid(!!!test_list)
  expect_equal(nrow(out), 4)
})

test_that("expand_grid can have inputs named 'unique', #718", {
  x <- 1:2
  y <- 1:2
  out <- expand_grid(x, unique = y)
  expect_equal(out$x, c(1, 1, 2, 2))
  expect_equal(out$unique, c(1, 2, 1, 2))
})

test_that("crossing works when only input is a data frame", {
  test_df <- tidytable(a = c("a", "a", "b"), b = c("a", "a", "b"))
  crossing_df <- crossing(test_df)
  unique_df <- unique(test_df)

  expect_equal(crossing_df, unique_df)
})

test_that("crossing works with data.frame inputs", {
  test_df <- tidytable(a = c("a", "b"), b = c("a", "b"))
  crossing_df <- crossing(test_df, x = 1:2)

  expect_named(crossing_df, c("a", "b", "x"))
  expect_equal(crossing_df$a, c("a", "a", "b", "b"))
  expect_equal(crossing_df$b, c("a", "a", "b", "b"))
  expect_equal(crossing_df$x, rep(1:2, 2))
})

test_that("expand_grid works with data.frame inputs", {
  test_df <- tidytable(a = c("b", "a"), b = c("b", "a"))
  expand_df <- expand_grid(test_df, x = 1:2)

  expect_named(expand_df, c("a", "b", "x"))
  expect_equal(expand_df$a, c("b", "b", "a", "a"))
  expect_equal(expand_df$b, c("b", "b", "a", "a"))
  expect_equal(expand_df$x, rep(1:2, 2))
})
