# Most tests are borrowed from
# https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-separate-wider.R

test_that("can extract columns", {
  df <- tidytable(x = "a123")
  out <- df %>% separate_wider_regex(x, c("a" = ".", "b" = "\\d+"))
  expect_equal(out, tidytable(a = "a", b = "123"))
})

test_that("works v2", {
  df <- tidytable(id = 1:3, x = c("m-123", "f-455", "f-123"))

  res <- df %>%
    separate_wider_regex(x, c(gender = ".", ".", unit = "\\d+"))

  check <- tidytable(id = 1:3,
                     gender = c("m", "f", "f"),
                     unit = c("123", "455", "123"))

  expect_equal(res, check)
})

test_that("can keep original columns", {
  df <- tidytable(id = 1:3, x = c("m-123", "f-455", "f-123"))

  res <- df %>%
    separate_wider_regex(x, c(gender = ".", ".", unit = "\\d+"),
                         cols_remove = FALSE)

  check <- tidytable(id = 1:3,
                     x = c("m-123", "f-455", "f-123"),
                     gender = c("m", "f", "f"),
                     unit = c("123", "455", "123"))

  expect_equal(res, check)
})

test_that("can use names_sep", {
  df <- tidytable(id = 1:3, x = c("m-123", "f-455", "f-123"))

  res <- df %>%
    separate_wider_regex(x, c(gender = ".", ".", unit = "\\d+"),
                         names_sep = "_")

  check <- tidytable(id = 1:3,
                     x_gender = c("m", "f", "f"),
                     x_unit = c("123", "455", "123"))

  expect_equal(res, check)
})

test_that("doesn't count NA input as problem", {
  df <- tidytable(x = NA)
  expect_equal(
    df %>% separate_wider_regex(x, patterns = c(a = ".", b = ".")),
    tidytable(a = NA_character_, b = NA_character_)
  )
})

test_that("can drop values", {
  df <- tidytable(x = "ab123")
  out <- df %>% separate_wider_regex(x, c("a" = ".", ".", "b" = "\\d+"))
  expect_equal(out, tidytable(a = "a", b = "123"))
})

test_that("works with empty data frames", {
  df <- tidytable(x = character())
  out <- separate_wider_regex(df, x, patterns = c(y = ".", z = "."))
  expect_equal(out, tidytable(y = character(), z = character()))
})
