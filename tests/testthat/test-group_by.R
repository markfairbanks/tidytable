test_that("group_by works", {
  df <- tidytable(x = c("a", "a", "b"),
                  y = c("a", "a", "b"),
                  z = 1:3)
  res <- group_by(df, x, y)
  expect_equal(group_vars(res), c("x", "y"))
  expect_true(is_grouped_df(res))
})

test_that("group_by works with .add", {
  df <- tidytable(x = c("a", "a", "b"),
                  y = c("a", "a", "b"),
                  z = 1:3)
  res <- df %>%
    group_by(x) %>%
    group_by(y, .add = TRUE)
  expect_equal(group_vars(res), c("x", "y"))
  expect_true(is_grouped_df(res))
})

test_that("works on rowwise_tt", {
  df <- rowwise(tidytable(x = 1:3, y = 1:3))
  res <- ungroup(df)
  expect_false(inherits(res, "rowwise_tt"))
})

test_that("ungroup works", {
  df <- tidytable(x = 1:3, y = 1:3) %>%
    group_by(x, y)
  res <- ungroup(df)
  expect_false(is_grouped_df(res))
  expect_equal(group_vars(res), NULL)
  res <- ungroup(df, y)
  expect_equal(group_vars(res), "x")
})
