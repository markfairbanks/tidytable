test_that("missings are filled correctly & doesn't modify-by-reference", {
  # filled down from last non-missing
  df <- data.table(x = c(NA, 1, NA, 2, NA, NA))

  out <- as_tidytable(df) %>% fill(x)
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))

  out <- as_tidytable(df) %>% fill(x, .direction = "up")
  expect_equal(out$x, c(1, 1, 2, 2, NA, NA))

  out <- as_tidytable(df) %>% fill(x, .direction = 'downup')
  expect_equal(out$x, c(1, 1, 1, 2, 2, 2))

  out <- as_tidytable(df) %>% fill(x, .direction = 'updown')
  expect_equal(out$x, c(1, 1, 2, 2, 2, 2))

  # modify-by-reference test
  expect_equal(df$x, c(NA, 1, NA, 2, NA, NA))
})

test_that("fill.works", {
  # filled down from last non-missing
  df <- data.table(x = c(NA, 1, NA, 2, NA, NA))

  out <- df %>% fill.(x)
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))
})

test_that("missings filled down for each atomic vector", {
  df <- data.table(
    lgl = c(T, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA)
  )

  out <- fill(df, lgl, int, dbl, chr)
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1, 1))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("works with data.frame", {
  df <- data.frame(
    lgl = c(T, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA),
    stringsAsFactors = FALSE
  )

  out <- fill(df, lgl, int, dbl, chr)
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1, 1))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("missings filled up for each vector", {
  df <- data.table(
    lgl = c(NA, T),
    int = c(NA, 1L),
    dbl = c(NA, 1),
    chr = c(NA, "a")
  )

  out <- fill(df, lgl, int, dbl, chr, .direction = "up")
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("doesn't modify-by-reference", {
  df <- data.table(
    lgl = c(NA, T),
    int = c(NA, 1L),
    dbl = c(NA, 1),
    chr = c(NA, "a")
  )

  fill(df, lgl, int, dbl, chr, .direction = "up")
  expect_equal(df$lgl, c(NA, T))
  expect_equal(df$int, c(NA, 1L))
  expect_equal(df$dbl, c(NA, 1))
  expect_equal(df$chr, c(NA, "a"))
})

test_that("fill respects grouping & is correct order", {
  df <- data.table(x = c(1, NA, NA), y = c(1, 1, 2))
  out <- fill(df, x, .by = y)

  expect_named(out, c("x", "y"))
  expect_equal(out$x, c(1, 1, NA))
})

test_that("works with grouped_tt", {
  df <- data.table(x = c(1, NA, NA), y = c(1, 1, 2))
  out <- df %>%
    group_by(y) %>%
    fill(x)

  expect_named(out, c("x", "y"))
  expect_equal(out$x, c(1, 1, NA))
  expect_equal(group_vars(out), "y")
})

#### Enhanced election

test_that("enhanced selection works is.numeric", {
  df <- data.table(
    lgl = c(T, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA)
  )

  out <- fill(df, where(is.numeric))
  expect_equal(out$lgl, c(TRUE, NA))
  expect_equal(out$int, c(1, 1))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", NA))
})

test_that("enhanced selection works", {
  df <- data.table(
    lgl = c(T, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA)
  )

  out <- fill(df, where(is.logical))
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1, NA))
  expect_equal(out$dbl, c(1, NA))
  expect_equal(out$chr, c("a", NA))
})

test_that("custom function works with quosure", {
  # filled down from last non-missing
  df <- data.table(x = c(NA, 1, NA, 2, NA, NA))

  fill_fn <- function(.df, col) {
    fill(.df, {{ col }})
  }

  out <- as_tidytable(df) %>% fill_fn(x)
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))
})
