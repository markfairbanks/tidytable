# slice_sample ------------------------------------------------------------
test_that("_sample works", {
  df <- tidytable(x = 1:8, y = rep(c("a", "b"), each = 4))
  res <- df %>%
    slice_sample(n = 2, .by = y)
  expect_equal(res$y, c("a", "a", "b", "b"))

  res <- df %>%
    slice_sample(prop = .5, .by = y)
  expect_equal(res$y, c("a", "a", "b", "b"))
})


test_that("_sample respects weight_by and replaces", {
  df <- tidytable(x = 1:100, wt = c(1, rep(0, 99)))

  out <- slice_sample(df, n = 1, weight_by = wt)
  expect_equal(out$x, 1)

  out <- slice_sample(df, n = 2, weight_by = wt, replace = TRUE)
  expect_equal(out$x, c(1, 1))
})

test_that("_sample edge cases", {
  df <- tidytable(x = 1:5)
  expect_equal(nrow(slice_sample(df, n = 6)), 5)

  # Returns 0 rows
  expect_equal(nrow(slice_sample(df, n = 0)), 0)
})

test_that("_sample works on grouped_tt", {
  df <- tidytable(x = 1:8, y = rep(c("a", "b"), each = 4))
  res <- df %>%
    group_by(y) %>%
    slice_sample(n = 2)
  expect_equal(res$y, c("a", "a", "b", "b"))

  res <- df %>%
    group_by(y) %>%
    slice_sample(prop = .5)
  expect_equal(res$y, c("a", "a", "b", "b"))
})
