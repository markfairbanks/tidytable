# These tests are borrowed from
# https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-separate-wider.R

test_that("can create column names", {
  df <- tidytable(x = c("a b", "x y"))
  out <- df %>% separate_wider_delim(x, " ", names_sep = "")
  expect_equal(out$x1, c("a", "x"))
  expect_equal(out$x2, c("b", "y"))
})

test_that("can ignore problems", {
  df <- tidytable(x = c("x", "x y", "x y z"))
  out <- df %>% separate_wider_delim(x, " ",
                                     names = c("a", "b"),
                                     too_few = "align_start",
                                     too_many = "drop",
  )
  expect_equal(out[1, ], tidytable(a = "x", b = NA_character_))
  expect_equal(out[3, ], tidytable(a = "x", b = "y"))
})

test_that("doesn't count NA input as problem", {
  df <- tidytable(x = NA)
  expect_equal(
    df %>% separate_wider_delim(x, ",", names = c("a", "b")),
    tidytable(a = NA_character_, b = NA_character_)
  )
})

test_that("validates its inputs", {
  df <- tidytable(x = "x")
  expect_error(df %>% separate_wider_delim())
  expect_error(df %>% separate_wider_delim(x))
})
