test_that("pulls last when empty", {
  df <- data.table(x = 1:3, y = 4:6)

  expect_equal(pull(df), 4:6)
})

test_that("returns a vec", {
  df <- data.table(x = 1:3, y = 4:6)
  res <- df %>%
    pull(y)

  expect_equal(df$y, res)
})

test_that("numerical selection works with negatives", {
  df <- data.table(x = 1:3, y = 4:6)
  res <- df %>%
    pull(-1)

  expect_equal(df$y, res)
})

test_that("numerical selection works with positives", {
  df <- data.table(x = c(1,2,3), y = c(4,5,6))
  res <- df %>%
    pull(2)

  expect_equal(df$y, res)
})

test_that("returns a vec with data.frame input", {
  df <- data.frame(x = c(1,2,3), y = c(4,5,6))
  res <- df %>%
    pull(y)

  expect_equal(df$y, res)
})

test_that("can use custom function with quosures", {
  df <- data.table(x = c(1,2,3), y = c(4,5,6))

  pull_fn <- function(data, col) {
    pull(data, {{ col }})
  }

  res <- df %>%
    pull_fn(y)

  expect_equal(df$y, res)
})

test_that("can return a named vector", {
  df <- data.frame(names = letters[1:3], values = 1:3)
  res <- df %>%
    pull(values, names)

  expected_vec <- 1:3
  names(expected_vec) <- letters[1:3]

  expect_equal(expected_vec, res)
})
