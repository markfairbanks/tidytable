test_that("works with scalar y", {
  vec <- 1:3
  res <- na_if(vec, 3)
  expect_equal(res, c(1, 2, NA))
})

test_that("works with vector y", {
  vec <- 1:3
  res <- na_if(vec, 3:1)
  expect_equal(res, c(1, NA, 3))
})

test_that("NAs are considered equal", {
  vec <- c(NA, 2, 3)
  res <- na_if(vec, NA)
  expect_equal(res, c(NA, 2, 3))
})

test_that("works on character", {
  vec <- c("a", "b")
  res <- na_if(vec, "a")
  expect_equal(res, c(NA, "b"))
})

test_that("follows vctrs recycling rules", {
  vec <- 1:3
  expect_error(na_if(vec, 1:2))
})

test_that("na_if. works", {
  vec <- 1:3
  res <- na_if.(vec, 3) %>% suppressWarnings()
  expect_equal(res, c(1, 2, NA))
})


