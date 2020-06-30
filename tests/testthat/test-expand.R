test_that("expand completes all values", {
  df <- data.table(x = 1:2, y = 1:2)
  out <- expand.(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("auto-converts data.frame inputs", {
  df <- data.frame(x = 1:2, y = 1:2)
  out <- expand.(df, x, y)
  expect_equal(nrow(out), 4)
})

test_that("preserves ordered factors", {
  df <- data.table(a = ordered("a"))
  out <- expand.(df, a)
  expect_equal(df$a, ordered("a"))
})

test_that("preserves NAs", {
  x <- c(NA, "A", "B")
  expect_equal(crossing.(x)$x, x)
  # expect_equal(nesting.(x)$x, x)
})

# test_that("NULL inputs", {
#   tb <- data.table(x = 1:5)
#   expect_equal(expand.(tb, x, y = NULL), tb)
#   # expect_equal(nesting(x = tb$x, y = NULL), tb)
#   expect_equal(crossing.(x = tb$x, y = NULL), tb)
# })

test_that("zero length input gives zero length output", {
  tb <- tidytable(x = character())
  expect_equal(expand.(tb, x), tb)

  expect_equal(
    expand_grid.(x = integer(), y = 1:3),
    tidytable(x = integer(), y = integer())
  )
})
