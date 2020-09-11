# vector ------------------------------------------------------------------

test_that("empty call does nothing", {
  x <- c(1, NA)
  expect_equal(replace_na.(x), x)
})

test_that("missing values are replaced", {
  x <- c(1, NA)
  expect_equal(replace_na.(x, 0), c(1, 0))
})

test_that("can only be length 0", {
  expect_error(replace_na.(1, 1:10))
})

test_that("works on numeric columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    mutate.(x = replace_na.(x, 5))

  expect_equal(replaced_df$x, c(1,2,5))
})

test_that("works on character columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    mutate.(y = replace_na.(y, "b"))

  expect_equal(replaced_df$y, c("a", "b", "c"))
})

# data frame -------------------------------------------------------------

test_that("empty call does nothing", {
  df <- tidytable(x = c(1, NA))
  out <- replace_na.(df)
  expect_equal(out, df)
})

test_that("missing values are replaced", {
  df <- tidytable(x = c(1, NA))
  out <- replace_na.(df, list(x = 0))
  expect_equal(out$x, c(1, 0))
})

test_that("missing values are replaced with correct type", {
  df <- tidytable(x = c(1, NA))
  out <- replace_na.(df, list(x = 0L))
  expect_equal(out$x, c(1, 0))
})

test_that("doesn't complain about variables that don't exist", {
  df <- tidytable(a = c(1, NA))
  out <- replace_na.(df, list(a = 100, b = 0))
  expect_equal(out, tidytable(a = c(1, 100)))
})

# test_that("can replace NULLs in list-column", {
#   df <- tidytable(x = list(1, NULL))
#   rs <- replace_na.(df, list(x = list(1:5)))
#
#   expect_identical(rs, tidytable(x = list(1, 1:5)))
# })


