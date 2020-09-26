test_that("group_split.() keeps the grouping variables by default", {
  tbl <- tidytable(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split.(tbl, g)

  expect_equal(as.data.table(res[[1]]), as.data.table(tbl[1:2,]))
  expect_equal(as.data.table(res[[2]]), as.data.table(tbl[3:4,]))
})


test_that("group_split() can discard the grouping variables with .keep = FALSE", {
  tbl <- tidytable(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split.(tbl, g, .keep = FALSE)

  expect_equal(as.data.table(res[[1]]), as.data.table(tbl[1:2, 1]))
  expect_equal(as.data.table(res[[2]]), as.data.table(tbl[3:4,1]))
})
