test_that("lag works on a vector", {

  x <- c(1:5)
  vec <- lag(x,1)

  expect_equal(vec, c(NA, 1,2,3,4))
})

test_that("lead works on a vector", {

  x <- c(1:5)
  vec <- lead(x,1)

  expect_equal(vec, c(2,3,4,5, NA))
})

test_that("lag works with mutate. on df", {

  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    mutate(x_lag = lag(x,1))

  expect_equal(df$x_lag, c(NA, 4,3,9))

})

test_that("lead works with mutate on df", {

  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    mutate(x_lead = lead(x,1))

  expect_equal(df$x_lead, c(3,9,7,NA))
})
