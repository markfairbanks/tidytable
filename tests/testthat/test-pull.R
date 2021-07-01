test_that("pulls last when empty", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))

  expect_equal(pull.(test_df), c(4,5,6))
})

test_that("returns a vec", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    pull.(y)

  expect_equal(test_df$y, result_vec)
})

test_that("numerical selection works with negatives", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    pull.(-1)

  expect_equal(test_df$y, result_vec)
})

test_that("numerical selection works with positives", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    pull.(2)

  expect_equal(test_df$y, result_vec)
})

test_that("returns a vec with data.frame input", {
  test_df <- data.frame(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    pull.(y)

  expect_equal(test_df$y, result_vec)
})

test_that("can use custom function with quosures", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))

  pull_fn <- function(data, col) {
    pull.(data, {{ col }})
  }

  result_vec <- test_df %>%
    pull_fn(y)

  expect_equal(test_df$y, result_vec)
})

test_that("can return a named vector", {
  test_df <- data.frame(names = letters[1:3], values = 1:3)
  result_vec <- test_df %>%
    pull.(values, names)

  expected_vec <- 1:3
  names(expected_vec) <- letters[1:3]

  expect_equal(expected_vec, result_vec)
})
