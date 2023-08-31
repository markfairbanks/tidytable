test_that("can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter(between(x, 1, 4))

  expect_equal(df$x, 1:4)
})

test_that("can filter a data.frame", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter(between(x, 1, 4))

  expect_equal(df$x, 1:4)
})

