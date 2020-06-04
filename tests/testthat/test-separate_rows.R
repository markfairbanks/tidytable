test_that("can handle collapsed rows", {
  df <- data.table(x = 1:3, y = c("a", "d,e,f", "g,h"))
  expect_equal(separate_rows.(df, y)$y, unlist(strsplit(df$y, "\\,")))
})

test_that("default pattern does not split decimals in nested strings", {
  df <- data.table(x = 1:3, y = c("1", "1.0,1.1", "2.1"))
  expect_equal(separate_rows.(df, y)$y, unlist(strsplit(df$y, ",")))
})

test_that("convert produces integers etc", {
  df <- data.table(x = "1,2,3", y = "T,F,T", z = "a,b,c")

  out <- separate_rows.(df, x, y, z, convert = TRUE)
  expect_equal(class(out$x), "integer")
  expect_equal(class(out$y), "logical")
  expect_equal(class(out$z), "character")
})

# test_that("leaves list columns intact (#300)", {
#   df <- data.table(x = "1,2,3", y = list(1))
#
#   out <- separate_rows.(df, x)
#   # Can't compare tibbles with list columns directly
#   expect_equal(names(out), c("x", "y"))
#   expect_equal(out$x, as.character(1:3))
#   expect_equal(out$y, rep(list(1), 3))
# })
