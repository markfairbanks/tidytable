test_that("works on a vector", {

  x <- c(1:4)
  vec <- lag(x,1)

  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    arrange.(x)

  expect_equal(df$x, c(3,4,7,9))
})
