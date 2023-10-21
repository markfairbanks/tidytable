test_that("works", {
  df <- tidytable(x = c("a", "b"), w = c(2, 1))
  expect_equal(uncount(df, w), tidytable(x = c("a", "a", "b")))
})

test_that("can request to preserve symbols", {
  df <- tidytable(x = 1, w = 1)
  expect_equal(uncount(df, w, .remove = FALSE), df)
})

test_that("unique identifiers created on request", {
  df <- tidytable(w = 1:3)
  expect_equal(uncount(df, w, .id = "id"), tidytable(id = c(1L, 1:2, 1:3)))
})
