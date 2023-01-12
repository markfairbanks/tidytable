test_that("can handle collapsed rows", {
  df <- data.table(x = 1:3, y = c("a", "d,e,f", "g,h"))
  result <- separate_longer_delim(df, y, ",")
  expect_equal(result$x, c(1, 2, 2, 2, 3, 3))
  expect_equal(result$y, c("a", "d", "e", "f", "g", "h"))
})

test_that("default pattern does not split decimals in nested strings", {
  df <- data.table(x = 1:3, y = c("1", "1.0,1.1", "2.1"))
  result <- separate_longer_delim(df, y, ",")
  expect_equal(result$y, c("1", "1.0", "1.1", "2.1"))
})

test_that("can split multiple columns & works on data.frame input", {
  df <- data.frame(id = 1:3, x = c("x", "x y", "x y z"), y = c("a", "a b", "a b c"))
  result <- df %>%
    separate_longer_delim(c(x, y), delim = " ")
  expect_equal(result$id, c(1, 2, 2, 3, 3, 3))
  expect_equal(result$x, c("x", "x", "y", "x", "y", "z"))
  expect_equal(result$y, c("a", "a", "b", "a", "b", "c"))
})
