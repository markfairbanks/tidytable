test_that("works on a single vector", {
  x <- c("a", "a", "b")
  expect_equal(n_distinct(x), 2)
})

test_that("works on multiple inputs", {
  x <- c(1, 1, 2)
  y <- c(1, 2, 1)
  expect_equal(n_distinct(x, y), 3)
})

test_that("na.rm works", {
  x <- c("a", "a", "b", NA)
  expect_equal(n_distinct(x, na.rm = TRUE), 2)
})

test_that("n_distinct. works", {
  x <- c("a", "a", "b")
  expect_equal(n_distinct.(x), 2)
})
