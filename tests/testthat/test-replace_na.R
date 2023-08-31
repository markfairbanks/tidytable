# vector ------------------------------------------------------------------
test_that("empty call does nothing", {
  x <- c(1, NA)
  expect_equal(replace_na(x), x)
})

test_that("missing values are replaced", {
  x <- c(1, NA)
  expect_equal(replace_na(x, 0), c(1, 0))
})

test_that("can only be length 1", {
  expect_error(replace_na(1, 1:10))
})

test_that("works on numeric columns", {
  x <- c(1, 2, NA)
  res <- replace_na(x, 5)

  expect_equal(res, c(1, 2, 5))
})

test_that("works on character columns", {
  df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  res <- df %>%
    mutate(y = replace_na(y, "b"))

  expect_equal(res$y, c("a", "b", "c"))
})

test_that("replace_na is converted to tidytable", {
  x <- c(1, NA)
  df <- tidytable(x = x)
  expect_equal(mutate(df, x = replace_na(x, 0))$x, c(1, 0))
})

# data frame -------------------------------------------------------------
test_that("empty call does nothing", {
  df <- tidytable(x = c(1, NA))
  out <- replace_na(df)
  expect_equal(out, df)
})

test_that("missing values are replaced", {
  df <- tidytable(x = c(1, NA), y = c(NA, 1))
  out <- replace_na(df, list(x = 0, y = 0))
  expect_equal(out$x, c(1, 0))
  expect_equal(out$y, c(0, 1))
})

test_that("missing values are replaced with correct type", {
  df <- tidytable(x = c(1, NA))
  out <- replace_na(df, list(x = 0L))
  expect_equal(out$x, c(1, 0))
})

test_that("doesn't complain about variables that don't exist", {
  df <- tidytable(a = c(1, NA))
  out <- replace_na(df, list(a = 100, b = 0))
  expect_equal(out, tidytable(a = c(1, 100)))
})

test_that("can replace NULLs in list-column", {
  df <- tidytable(x = list(1, NULL))
  rs <- replace_na(df, list(x = list(1:5)))

  expect_identical(rs, tidytable(x = list(1, 1:5)))
})


