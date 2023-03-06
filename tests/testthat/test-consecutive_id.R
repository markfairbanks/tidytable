test_that("works with simple vectors", {
  expect_equal(consecutive_id(c(1, 1, 2, 1, 2)), c(1, 1, 2, 3, 4))
})

test_that("handles data frames", {
  df <- tidytable(x = c(1, 1, 1, 1), y = c(1, 2, 2, 1))
  expect_equal(consecutive_id(df), c(1, 2, 2, 3))
})

test_that("follows recycling rules", {
  expect_equal(consecutive_id(double(), 1), integer())
  expect_equal(consecutive_id(1:2, 1), 1:2)
})

test_that("consecutive_id. works", {
  expect_equal(suppressWarnings(consecutive_id.(c(1, 1, 2, 1, 2))), c(1, 1, 2, 3, 4))
})
