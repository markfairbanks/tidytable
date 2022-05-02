test_that("empty count.() returns number of rows", {
  df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  out <- df %>%
    count.() %>%
    suppressWarnings()

  expect_named(out, c("n"))
  expect_equal(out$n, nrow(df))
})

test_that("works on data.frame", {
  df <- data.frame(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  out <- df %>%
    count.()

  expect_named(out, c("n"))
  expect_equal(out$n, nrow(df))
})

test_that("count.(group) returns group results", {
  df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  out <- df %>%
    count.(d)

  expect_named(out, c("d", "n"))
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$n, c(2, 1))
})

test_that("works with tidyselect", {
  df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  out <- df %>%
    count.(where(is.character))

  expect_named(out, c("c", "d", "n"))
  expect_equal(out$c, c("a", "a"))
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$n, c(2, 1))
})

test_that("can specify the name", {
  df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))
  out <- df %>%
    count.(name = "N")

  expect_named(out, c("N"))
})

test_that("can make a function with quosures", {
  df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "a"), d = c("a", "a", "b"))

  count_fn <- function(.df, col) {
    count.(.df, {{ col }})
  }

  out <- df %>%
    count_fn(d)

  expect_named(out, c("d", "n"))
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$n, c(2, 1))
})
