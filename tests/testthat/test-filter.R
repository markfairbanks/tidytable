test_that("dt_ can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    dt_filter(x <= 4)

  expect_equal(df$x, 1:4)
})

test_that("can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter.(x <= 4)

  expect_equal(df$x, 1:4)
})

test_that("can filter a data.frame", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter.(x <= 4)

  expect_equal(df$x, 1:4)
})

test_that("can filter multiple conditions with commas", {
  df <- data.table(x = 1:10, y = 1:10)

  df <- df %>%
    filter.(x <= 4, y < 3)

  expect_equal(df$x, 1:2)
  expect_equal(df$y, 1:2)
})

test_that("can filter with |", {
  df <- data.table(x = 1:10, y = 1:10)

  df <- df %>%
    filter.(x <= 4 | y < 3)

  expect_equal(df$x, 1:4)
  expect_equal(df$y, 1:4)
})

test_that("filter works with 'by'", {
  df <- data.table(x = c(1, 1, 2, 2), y = c("a", "a", "a", "b"))

  df <- df %>%
    filter.(x == mean(x), by = y)

  expect_equal(df$x, 2)
  expect_equal(df$y, "b")
})
