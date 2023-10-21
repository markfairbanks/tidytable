test_that("works with true = NA", {
  x <- 1:4

  res <- if_else(x > 2L, NA, x - 1L)

  expect_equal(res, c(0, 1, NA, NA))
})

test_that("works with false = NA", {
  x <- 1:4

  res <- if_else(x > 2L, x - 1L, NA)

  expect_equal(res, c(NA, NA, 2, 3))
})
