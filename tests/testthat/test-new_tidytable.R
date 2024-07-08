test_that("can create a tidytable from a list", {
  l <- list(x = 1:3, y = c("a", "a", "b"))
  out <- new_tidytable(l)
  expect_named(out, c("x", "y"))
  expect_true(is_tidytable(out))
  expect_true(inherits(out, "tbl"))
})
