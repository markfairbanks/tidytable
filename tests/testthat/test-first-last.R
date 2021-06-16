test_that("work on vectors", {
  expect_equal(first.(letters), "a")
  expect_equal(last.(letters), "z")
})

test_that("work on data frames", {
  df <- tidytable(x = 1:3, y = 1:3)
  expect_equal(first.(df), head(df, 1))
  expect_equal(last.(df), tail(df, 1))
})
