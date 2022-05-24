test_that("works when length(y) == 1", {
  vec <- 1:3
  res <- na_if.(vec, 3)
  expect_equal(res, c(1, 2, NA))
})

test_that("works when length(y) > 1", {
  vec <- 1:3
  res <- na_if.(vec, c(1, 3))
  expect_equal(res, c(NA, 2, NA))
})
