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

test_that("works with dot", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter(suppressWarnings(between.(x, 1, 4)))

  expect_equal(df$x, 1:4)
})
