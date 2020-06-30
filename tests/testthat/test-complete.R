test_that("complete with no variables returns data as is", {
  expect_equal(complete.(mtcars), mtcars)
})

test_that("basic invocation works", {
  df <- tidytable(x = 1:2, y = 1:2, z = 3:4)
  out <- complete.(df, x, y)
  expect_equal(nrow(out), 4)
  expect_equal(out$z, c(3, NA, NA, 4))
})

test_that("empty expansion returns original", {
  df <- tidytable(x = character())
  rs <- complete.(df, y = NULL)
  expect_equal(rs, df)

  df <- tidytable(x = 1:4)
  rs <- complete.(df, y = NULL)
  expect_equal(rs, df)
})
