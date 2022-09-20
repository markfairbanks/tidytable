test_that("group_by works", {
  df <- tidytable(x = c("a", "a", "b"),
                  y = c("a", "a", "b"),
                  z = 1:3)
  res <- group_by(df, x, y)
  expect_equal(group_vars(res), c("x", "y"))
  expect_true(is_grouped_df(res))
})

test_that("works with dot", {
  df <- tidytable(x = c("a", "a", "b"),
                  y = c("a", "a", "b"),
                  z = 1:3)
  res <- group_by.(df, x, y)
  expect_equal(group_vars.(res), c("x", "y"))
  expect_true(is_grouped_df.(res))
})

test_that("works on rowwise_tt", {
  df <- rowwise(tidytable(x = 1:3, y = 1:3))
  res <- ungroup(df)
  expect_false(inherits(res, "rowwise_tt"))
})
