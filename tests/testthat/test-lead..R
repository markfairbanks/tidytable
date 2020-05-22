test_that("lags. works on a vector", {

  x <- c(1:5)
  vec <- lags.(x,1)

  expect_equal(vec, c(NaN, 1,2,3,4))
})

test_that("leads. works on a vector", {

  x <- c(1:5)
  vec <- leads.(x,1)

  expect_equal(vec, c(2,3,4,5, NaN))
})

test_that("lags. works with mutate. on df", {

  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    mutate.(x_lag = lags.(x,1))

  expect_equal(df$x_lag, c(NA, 4,3,9))

})

test_that("leads. works with mutate. on df", {

  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    mutate.(x_lead = leads.(x,1))

  expect_equal(df$x_lead, c(3,9,7,NA))

})
