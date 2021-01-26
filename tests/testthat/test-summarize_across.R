defaultW <- getOption("warn")
options(warn = -1)

test_that("single function works", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), mean, na.rm = TRUE)

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 2)
  expect_equal(result_df$b, 5)
})

test_that("can use anonymous functions", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), ~ mean(.x, na.rm = TRUE))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 2)
  expect_equal(result_df$b, 5)
})

test_that("can use other columns", {
  test_df <- tidytable(a = 1:3, b = 1:3, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), ~ mean(.x)/mean(b))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 1)
  expect_equal(result_df$b, 1)
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

  expect_named(result_df, c("a_avg", "b_avg", "a_max", "b_max"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_max, 3)
  expect_equal(result_df$b_max, 6)

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = ~ mean(.x), max = ~ max(.x)))

  expect_named(result_df, c("a_avg", "b_avg", "a_max", "b_max"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_max, 3)
  expect_equal(result_df$b_max, 6)
})

test_that("can pass list of functions with no names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(mean, max))

  expect_named(result_df, c("a_1", "b_1", "a_2", "b_2"))
  expect_equal(result_df$a_1, 2)
  expect_equal(result_df$b_1, 5)
  expect_equal(result_df$a_2, 3)
  expect_equal(result_df$b_2, 6)
})

test_that("can pass list of functions with some names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max))

  expect_named(result_df, c("a_avg", "b_avg", "a_2", "b_2"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_2, 3)
  expect_equal(result_df$b_2, 6)
})

test_that("can pass list of named functions with .by and .names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max = max), .by = z, .names = "{.fn}_{.col}")

  expect_named(result_df, c("z", "avg_a", "avg_b", "max_a", "max_b"))
  expect_equal(result_df$avg_a, c(1.5, 3))
  expect_equal(result_df$avg_b, c(4.5, 6))
  expect_equal(result_df$max_a, c(2, 3))
  expect_equal(result_df$max_b, c(5, 6))
})

test_that("can pass list of named functions with .by and .names using fn and col", {

  # This test will need to be removed when {col} and {fn} is deprecated

  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize_across.(c(a, b), list(avg = mean, max = max), .by = z, .names = "{fn}_{col}")

  expect_named(result_df, c("z", "avg_a", "avg_b", "max_a", "max_b"))
  expect_equal(result_df$avg_a, c(1.5, 3))
  expect_equal(result_df$avg_b, c(4.5, 6))
  expect_equal(result_df$max_a, c(2, 3))
  expect_equal(result_df$max_b, c(5, 6))
})

options(warn = defaultW)
