test_that("can arrange the dataset", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)
  df <- df %>%
    arrange.(x)

  expect_equal(df$x, c(3, 4, 7, 9))
})

test_that("can convert and arrange a data.frame", {
  df <- data.frame(x = c(4, 3, 9, 7), y = 1:4)
  df <- df %>%
    arrange.(x)

  expect_equal(df$x, c(3, 4, 7, 9))
})

test_that("can arrange the dataset descending", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)
  df <- df %>%
    arrange.(-x)

  expect_equal(df$x, c(9, 7, 4, 3))
})

test_that("can arrange the dataset with desc.()", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)

  desc_df <- df %>%
    arrange.(desc.(x))

  check_df <- df %>%
    arrange.(-x)

  expect_equal(desc_df, check_df)
})

test_that("can arrange the dataset with desc.() on chr", {
  df <- data.table(x = c("a", "a", "b"), y = 1:3)

  desc_df <- df %>%
    arrange.(desc.(x))

  check_df <- df %>%
    arrange.(-x)

  expect_equal(desc_df, check_df)
})

test_that("desc works with internal quosure", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)

  desc_col <- quo(x)
  desc_expr <- expr(desc(!!desc_col))

  desc_df <- df %>%
    arrange.(!!desc_expr)

  check_df <- df %>%
    arrange.(-x)

  expect_equal(desc_df, check_df)
})

test_that("desc wworks with .data pronoun", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)

  desc_df <- df %>%
    arrange.(desc(.data$x))

  check_df <- df %>%
    arrange.(-x)

  expect_equal(desc_df, check_df)
})

test_that("can arrange with multiple conditions", {
  df <- data.table(x = c(4, 3, 2, 1), y = c("a", "a", "b", "b"))
  df <- df %>%
    arrange.(y, x)

  expect_equal(df$x, c(3, 4, 1, 2))
})

test_that("can make function with quosures", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  arrange_fn <- function(.df, col) {
    .df %>%
      arrange.({{col}})
  }

  df <- df %>%
    arrange_fn(-x)

  expect_equal(df$x, c(3, 2, 1))
  expect_equal(df$y, c("b", "a", "a"))
})

test_that("can use .data", {
  df <- data.table(x = c(4, 3, 9, 7), y = 1:4)
  df <- df %>%
    arrange.(.data$x)

  expect_equal(df$x, c(3, 4, 7, 9))
})

test_that("can use a transmute expression", {
  df <- data.table(x = 1:3, y = 1:3, z = 3:1)
  res <- df %>%
    arrange.(x - y, z)

  expect_equal(res$x, 3:1)
})

test_that("can use `.env`", {
  df <- data.table(x = c(4, 3, 9, 7))
  y <- 1
  res <- df %>%
    arrange.(x + .env$y)

  expect_equal(res$x, c(3, 4, 7, 9))
})

test_that("properly orders NAs, #541", {
  df <- data.table(x = c(NA, "b", "a"))
  res <- df %>%
    arrange.(x)

  expect_equal(res$x, c("a", "b", NA))
})
