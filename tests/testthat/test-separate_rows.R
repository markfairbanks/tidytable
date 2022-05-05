test_that("can handle collapsed rows", {
  df <- data.table(x = 1:3, y = c("a", "d,e,f", "g,h"))
  result <- separate_rows.(df, y)
  expect_equal(result$x, c(1, 2, 2, 2, 3, 3))
  expect_equal(result$y, c("a", "d", "e", "f", "g", "h"))
})

test_that("default pattern does not split decimals in nested strings", {
  df <- data.table(x = 1:3, y = c("1", "1.0,1.1", "2.1"))
  result <- separate_rows.(df, y)
  expect_equal(result$y, c("1", "1.0", "1.1", "2.1"))
})

test_that("convert produces integers etc & works with all cols", {
  df <- data.table(x = "1,2,3", y = "T,F,T", z = "a,b,c")
  result <- separate_rows.(df, x, y, z, convert = TRUE)
  expect_true(is.integer(result$x))
  expect_true(is.logical(result$y))
  expect_true(is.character(result$z))
})
