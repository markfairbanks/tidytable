# test_that("all missings left unchanged", {
#   df <- data.table::data.table(
#     lgl = c(NA, NA),
#     int = c(NA_integer_, NA),
#     dbl = c(NA_real_, NA),
#     chr = c(NA_character_, NA)
#   )
#
#   down <- dt_fill(df, lgl, int, dbl, chr)
#   up <- dt_fill(df, lgl, int, dbl, chr, .direction = "up")
#
#   expect_identical(down, df)
#   expect_identical(up, df)
# })

test_that("missings are filled correctly", {
  # filled down from last non-missing
  df <- data.table::data.table(x = c(NA, 1, NA, 2, NA, NA))

  out <- as_dt(df) %>% dt_fill(x)
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))

  out <- as_dt(df) %>% dt_fill(x, .direction = "up")
  expect_equal(out$x, c(1, 1, 2, 2, NA, NA))

  out <- as_dt(df) %>% dt_fill(x, .direction = 'downup')
  expect_equal(out$x, c(1, 1, 1, 2, 2, 2))

  out <- as_dt(df) %>% dt_fill(x, .direction = 'updown')
  expect_equal(out$x, c(1, 1, 2, 2, 2, 2))
})

test_that("missings filled down for each atomic vector", {
  df <- data.table::data.table(
    lgl = c(T, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA),
    lst = list(1:5, NULL)
  )

  out <- dt_fill(df, lgl, int, dbl, chr, lst)
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1, 1))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("missings filled up for each vector", {
  df <- data.table::data.table(
    lgl = c(NA, T),
    int = c(NA, 1L),
    dbl = c(NA, 1),
    chr = c(NA, "a")
  )

  out <- dt_fill(df, lgl, int, dbl, chr, .direction = "up")
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("fill preserves attributes", {
  df <- data.table::data.table(x = factor(c(NA, "a", NA)))

  out_d <- as_dt(df) %>% dt_fill(x)
  out_u <- as_dt(df) %>% dt_fill(x, .direction = "up")

  expect_equal(attributes(out_d$x), attributes(df$x))
  expect_equal(attributes(out_u$x), attributes(df$x))
})

test_that("fill respects grouping", {
  df <- data.table::data.table(x = c(1, 1, 2), y = c(1, NA, NA))
  out <- dt_fill(df, y, by = x)
  expect_equal(out$y, c(1, 1, NA))
})
