test_that("works on a single vector", {
  x <- c("a", "a", "b")
  expect_equal(n_distinct.(x), 2)
})

test_that("works on multiple vectors", {
  x <- c("a", "a", "b")
  y <- c("a", "b", "c", NA)
  expect_equal(n_distinct.(x, y), 4)
})


test_that("na.rm works", {
  x <- c("a", "a", "b")
  y <- c("a", "b", "c", NA)
  expect_equal(n_distinct.(x, y, na.rm = TRUE), 3)
})
