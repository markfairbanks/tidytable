test_that("can nest all data", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by()

  expect_named(result_df, c("data"))

  result_df <- test_df %>%
    nest_by(.key = "stuff")

  expect_named(result_df, c("stuff"))
  expect_equal(nrow(result_df), 1)
})

test_that("works with dot", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by.()

  expect_named(result_df, c("data"))
})

test_that("can nest by group", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by(c)

  expect_named(result_df, c("c", "data"))

  result_df <- test_df %>%
    nest_by(c, .key = "stuff")

  expect_named(result_df, c("c", "stuff"))
  expect_equal(class(result_df$stuff), "list")
  expect_equal(nrow(result_df), 2)
})

test_that(".keep works", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))

  result_df <- test_df %>%
    nest_by(c, d, .keep = TRUE, .key = "stuff") %>%
    mutate(num_cols = map_dbl(stuff, ncol))

  expect_named(result_df, c("c", "d", "stuff", "num_cols"))
  expect_equal(result_df$num_cols, c(4, 4))
  expect_equal(nrow(result_df), 2)
})

test_that("can nest by group with quosure function", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  nest_by_fn <- function(.df, col, ...) {
    nest_by(.df, {{ col }}, ...)
  }

  result_df <- test_df %>%
    nest_by_fn(c)

  expect_named(result_df, c("c", "data"))

  result_df <- test_df %>%
    nest_by_fn(c, .key = "stuff")

  expect_named(result_df, c("c", "stuff"))
  expect_equal(class(result_df$stuff), "list")
  expect_equal(nrow(result_df), 2)
})
