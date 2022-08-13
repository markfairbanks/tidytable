test_that("%in% works", {
  expect_equal(c("a", "d") %in% c("a", "b"), c(TRUE, FALSE))
})

test_that("%notin% works", {
  expect_equal(c("a", "d") %notin% c("a", "b"), c(FALSE, TRUE))
})

test_that("properly handles character comparison with NA", {
  expect_equal(c("a", "b") %in% NA, c(FALSE, FALSE))
})
