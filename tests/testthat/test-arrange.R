test_that("can arrange the dataset", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    dt_arrange(x)

  expect_equal(df$x, c(3,4,7,9))
})

test_that("can convert and arrange a data.frame", {
  df <- data.frame(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    dt_arrange(x)

  expect_equal(df$x, c(3,4,7,9))
})

test_that("can arrange the dataset descending", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    dt_arrange(-x)

  expect_equal(df$x, c(9,7,4,3))
})

test_that("can arrange with multiple conditions", {
  df <- data.table(x = c(4,3,2,1), y = c("a","a","b","b"))
  df <- df %>%
    dt_arrange(y, x)

  expect_equal(df$x, c(3,4,1,2))
})
