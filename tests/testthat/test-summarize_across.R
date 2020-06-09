test_that("single function works", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), mean, na.rm = TRUE)

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 2)
  expect_equal(result_df$b, 5)
})

test_that("summarise spelling works", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarise_across.(c(a, b), mean, na.rm = TRUE)

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 2)
  expect_equal(result_df$b, 5)
})

test_that("single function works with .by", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), mean, na.rm = TRUE, .by = z)

  expect_named(result_df, c("z", "a", "b"))
  expect_equal(result_df$a, c(1.5, 3))
  expect_equal(result_df$b, c(4.5, 6))
})

test_that("can pass list of named functions", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max = max))

  expect_named(result_df, c("avg_a", "avg_b", "max_a", "max_b"))
  expect_equal(result_df$avg_a, 2)
  expect_equal(result_df$avg_b, 5)
  expect_equal(result_df$max_a, 3)
  expect_equal(result_df$max_b, 6)
})

test_that("can pass unnamed list of functions", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max))

  expect_named(result_df, c("avg_a", "avg_b", "fn_a", "fn_b"))
  expect_equal(result_df$avg_a, 2)
  expect_equal(result_df$avg_b, 5)
  expect_equal(result_df$fn_a, 3)
  expect_equal(result_df$fn_b, 6)
})

test_that("can pass list of named functions with .by", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max = max), .by = z)

  expect_named(result_df, c("z", "avg_a", "avg_b", "max_a", "max_b"))
  expect_equal(result_df$avg_a, c(1.5, 3))
  expect_equal(result_df$avg_b, c(4.5, 6))
  expect_equal(result_df$max_a, c(2, 3))
  expect_equal(result_df$max_b, c(5, 6))
})
