# %in% ----------------------------------------------------

test_that("%in% works", {
  expect_equal(c("a", "d") %in% c("a", "b"), c(TRUE, FALSE))
})

test_that("%in% falls back to base implementation on arguments that don't share a prototype", {
  expect_equal(c("1", "4") %in% c(1, 2), c(TRUE, FALSE))
})

test_that("properly handles character comparison with NA", {
  expect_equal(c("a", "b") %in% NA, c(FALSE, FALSE))
})

test_that("can compare to a list, #565", {
  expect_equal(c(1, 2) %in% list(1), c(TRUE, FALSE))
})

# %notin% ----------------------------------------------------

test_that("%notin% works", {
  expect_equal(c("a", "d") %notin% c("a", "b"), c(FALSE, TRUE))
})
