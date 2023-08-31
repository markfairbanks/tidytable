test_that("mutate_rowwise() adds column", {
  test_df <- data.table(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise(row_mean = mean(c(x, y)))

  expect_equal(results_df$row_mean, 1:3)
})

test_that("mutate_rowwise() doesn't modify by reference", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise(x = x + y)

  expect_equal(test_df$x, c(1, 2, 3))
  expect_equal(results_df$x, c(5, 7, 9))
})

test_that("can use .keep and relocate", {
  test_df <- data.table(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise(row_mean = mean(c(x, y)),
                    .keep = "used",
                    .before = x)

  expect_named(results_df, c("row_mean", "x", "y"))
  expect_equal(results_df$row_mean, 1:3)
})

test_that("c_across provides all columns", {
  test_df <- data.table(x = 1:3, y = 4:6)

  results_df <- test_df %>%
    mutate_rowwise(row_mean = mean(c_across()))
  results_df_every <- test_df %>%
    mutate_rowwise(row_mean = mean(c_across(everything())))

  expect_equal(results_df$row_mean, results_df_every$row_mean)
})

test_that("c_across cols selection works", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise(row_mean = mean(c_across(cols = where(is.numeric))))

  expect_equal(results_df$row_mean, c(2.5, 3.5, 4.5))
})

test_that("c_across works with space named columns", {
  test_df <- data.table(`x y`=1:3, `x z`=1, y=3)
  result_df <- test_df %>%
    mutate_rowwise(sum = sum(c_across(contains(" "))))

  expect_equal(result_df$sum, c(2, 3, 4))
})

test_that("overrides grouped_tt", {
  test_df <- tidytable(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  results_df <- test_df %>%
    group_by(z) %>%
    mutate_rowwise(row_mean = mean(c(x, y))) %>%
    suppressWarnings()

  expect_equal(results_df$row_mean, 1:3)
})
