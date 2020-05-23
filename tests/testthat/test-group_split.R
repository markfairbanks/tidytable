test_that("group_split.() keeps the grouping variables by default", {
  tbl <- tidytable(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split.(tbl, g)

  expect_equal(res, list(tbl[1:2,], tbl[3:4,]))
  expect_identical(res, list(tbl[1:2,], tbl[3:4,]))
})


test_that("group_split() can discard the grouping variables with .keep = FALSE", {
  tbl <- tidytable(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split.(tbl, g, .keep = FALSE)

  expect_identical(res, list(tbl[1:2, 1, drop = FALSE], tbl[3:4,1, drop = FALSE]))
})
