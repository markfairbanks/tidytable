test_that("can use if_all", {
  test_df <- tidytable(x = rep(1, 3), y = 1:3, z = c("a", "a", "b"))

  out <- test_df %>% filter(if_all(c(x, y), ~ .x < 2))

  expect_equal(out$y, 1)
})

test_that("can use if_all.", {
  test_df <- tidytable(x = rep(1, 3), y = 1:3, z = c("a", "a", "b"))

  out <- test_df %>% filter(if_all.(c(x, y), ~ .x < 2))

  expect_equal(out$y, 1)
})

test_that("can use with other filters", {
  test_df <- tidytable(x = rep(1, 3), y = 1:3, z = c("a", "a", "b"))

  out <- test_df %>% filter(z == "b", if_all(c(x, y), ~ .x < 2))

  expect_equal(nrow(out), 0)
})

test_that("can use if_any", {
  test_df <- tidytable(x = rep(1, 3), y = rep(2, 3), z = c("a", "a", "b"))

  out <- test_df %>% filter(if_any(c(x, y), ~ .x == 3))

  expect_equal(nrow(out), 0)
})

test_that("can use if_any.", {
  test_df <- tidytable(x = rep(1, 3), y = rep(2, 3), z = c("a", "a", "b"))

  out <- test_df %>% filter(if_any(c(x, y), ~ .x == 3))

  expect_equal(nrow(out), 0)
})

test_that("can filter using another column", {
  test_df <- tidytable(x = 1:3, y = rep(3, 3))

  out <- test_df %>% filter(if_any(x, ~ .x == y))

  expect_equal(out$x, 3)
})

test_that("can use a bare function", {
  test_df <- tidytable(x = c(1:2, NA), y = rep(3, 3))

  out <- test_df %>% filter(if_any(x, is.na))

  expect_equal(out$y, 3)
})

test_that("can filter using a pre-defined variable", {
  test_df <- tidytable(x = 1:3, y = rep(3, 3))

  filter_val <- 3

  out <- test_df %>% filter(if_any(x, ~ .x == filter_val))

  expect_equal(out$x, 3)
})

test_that("doesn't use .by columns, can use n()", {
  test_df <- tidytable(
    group = c("a", "a", "b"),
    val1 = 1:3,
    val2 = 1:3
  )

  out <- test_df %>%
    filter(if_any(everything(), ~ .x <= n()), .by = group)

  expect_equal(nrow(out), 2)
})

test_that("can be used in a custom function", {
  test_df <- tidytable(x = 1:3, y = rep(3, 3))

  filter_if_any <- function(data, cols, filter_val) {
    data %>%
      filter(if_any({{ cols }}, ~ .x == filter_val))
  }

  out <- test_df %>% filter_if_any(x, 3)

  expect_equal(out$x, 3)
})
