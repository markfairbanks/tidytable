test_that("non-missing scalar replaces all missing values", {
  x <- c(NA, 1)
  expect_equal(coalesce.(x, 1), c(1, 1))
})

test_that("coerces to common type", {
  expect_identical(coalesce.(NA, 1), 1)

  f <- factor("x", levels = c("x", "y"))
  expect_identical(coalesce.(NA, f), f)
})

test_that("finds non-missing values in multiple positions", {
  x1 <- c(1L, NA, NA)
  x2 <- c(NA, 2L, NA)
  x3 <- c(NA, NA, 3L)

  expect_equal(coalesce.(x1, x2, x3), 1:3)
})

test_that("can specify ptype", {
  x <- NA
  expect_equal(coalesce.(x, 1, .ptype = integer()), 1L)
})

test_that("can specify output size", {
  x <- NA
  expect_equal(coalesce.(x, 1, .size = 2), c(1, 1))
})
