test_that("works with no dots", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  drop_df <- test_df %>%
    drop_na()

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, 1)
  expect_equal(drop_df$y, "a")
})

test_that("works on a data.frame", {
  test_df <- data.frame(x = c(1, 2, NA), y = c("a", NA, "b"),
                        stringsAsFactors = FALSE)

  drop_df <- test_df %>%
    drop_na()

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, 1)
  expect_equal(drop_df$y, "a")
})

test_that("works with one dot", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  drop_df <- test_df %>%
    drop_na(x)

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, c(1, 2))
  expect_equal(drop_df$y, c("a", NA))
})

test_that("works with multiple dots", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  drop_df <- test_df %>%
    drop_na(x, y)

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, 1)
  expect_equal(drop_df$y, "a")
})

test_that("works with select helpers", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  drop_df <- test_df %>%
    drop_na(starts_with("x"))

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, c(1, 2))
  expect_equal(drop_df$y, c("a", NA))
})

test_that("works with quosures", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))

  drop_na_fn <- function(.df, col) {
    drop_na(.df, {{ col }})
  }

  drop_df <- test_df %>%
    drop_na_fn(x)

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, c(1, 2))
  expect_equal(drop_df$y, c("a", NA))
})

test_that("drop_na. works", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  drop_df <- test_df %>%
    drop_na.()

  expect_named(drop_df, c("x", "y"))
  expect_equal(drop_df$x, 1)
  expect_equal(drop_df$y, "a")
})
