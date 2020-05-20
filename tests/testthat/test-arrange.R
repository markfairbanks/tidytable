test_that("can arrange the dataset", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    arrange.(x)

  expect_equal(df$x, c(3,4,7,9))
})

test_that("dt_ can arrange the dataset", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    dt_arrange(x)

  expect_equal(df$x, c(3,4,7,9))
})

test_that("can convert and arrange a data.frame", {
  df <- data.frame(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    arrange.(x)

  expect_equal(df$x, c(3,4,7,9))
})

test_that("can arrange the dataset descending", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)
  df <- df %>%
    arrange.(-x)

  expect_equal(df$x, c(9,7,4,3))
})

test_that("can arrange the dataset with desc.()", {
  df <- data.table(x = c(4,3,9,7), y = 1:4)

  desc_df <- df %>%
    arrange.(desc.(x))

  check_df <- df %>%
    arrange.(-x)

  expect_equal(desc_df, check_df)
})

test_that("can arrange with multiple conditions", {
  df <- data.table(x = c(4,3,2,1), y = c("a","a","b","b"))
  df <- df %>%
    arrange.(y, x)

  expect_equal(df$x, c(3,4,1,2))
})

test_that("can make function with quosures", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  arrange_fn <- function(.df, col) {
    .df %>%
      arrange.({{col}})
  }

  df <- df %>%
    arrange_fn(-x)

  expect_equal(df$x, c(3,2,1))
  expect_equal(df$y, c("b", "a", "a"))
})
