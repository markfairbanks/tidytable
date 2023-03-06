test_that("with no variables returns data as tidytable", {
  expect_equal(complete(mtcars), as_tidytable(mtcars))
})

test_that("basic invocation works", {
  df <- tidytable(x = 1:2, y = 1:2, z = 3:4)
  out <- complete(df, x, y)
  expect_equal(nrow(out), 4)
  expect_equal(out$z, c(3, NA, NA, 4))
})

test_that("empty expansion returns original", {
  df <- tidytable(x = character())
  rs <- complete(df, y = NULL)
  expect_equal(rs, df)

  df <- tidytable(x = 1:4)
  rs <- complete(df, y = NULL)
  expect_equal(rs, df)
})

test_that("not drop unspecified levels in complete", {
  df <- tidytable(x = 1:3, y = 1:3, z = c("a", "b", "c"))
  df2 <- df %>% complete(z = c("a", "b"))
  expect <- df[, c("z", "x", "y")]
  expect_equal(df2, expect)
})

test_that("complete. works", {
  df <- tidytable(x = 1:2, y = 1:2, z = 3:4)
  out <- suppressWarnings(complete.(df, x, y))
  expect_equal(nrow(out), 4)
  expect_equal(out$z, c(3, NA, NA, 4))
})
