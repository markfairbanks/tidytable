test_that("works", {
  df <- data.table(x = 1:3, y = 1:3, z = c("a", "a", "b"))
  res <- rowwise(df)
  expect_true(inherits(res, "rowwise_tt"))
})
