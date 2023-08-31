# Empty `row_number()` in `mutate()` ----------------------------------
test_that("row_number() in mutate works", {
  df <- data.table(x = c(2, 1, 3))
  df <- df %>%
    mutate(row_num = row_number())

  expect_equal(df$row_num, c(1, 2, 3))
})

test_that("row_number works on 0 row data frame, #639", {
  df <- data.table(x = integer(), y = character())
  res<- df %>%
    mutate(x = row_number())

  expect_equal(res$x, integer())
})

test_that("row_number() works with .by", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(row = row_number(), .by = x)

  expect_equal(df$row, c(1, 1, 1))
})

# dplyr tests ------------------------------------------------
test_that("ranking empty vector returns empty vector", {
  x <- numeric()

  expect_equal(row_number(x), numeric())
  expect_equal(min_rank(x), numeric())
  expect_equal(dense_rank(x), numeric())
  expect_equal(percent_rank(x), numeric())
  expect_equal(cume_dist(x), numeric())
  # expect_equal(ntile(x, 1), numeric())
})

test_that("rank functions deal pass NA (and NaN) through", {
  x <- c(1, 2, NA, 1, 0, NaN)

  expect_equal(percent_rank(x), c(1 / 3, 1, NA, 1 / 3, 0, NA))
  expect_equal(min_rank(x), c(2L, 4L, NA, 2L, 1L, NA))
  expect_equal(dense_rank(x), c(2L, 3L, NA, 2L, 1L, NA))
  expect_equal(cume_dist(x), c(.75, 1, NA, .75, .25, NA))
  expect_equal(row_number(x), c(2L, 4L, NA, 3L, 1L, NA))
})

test_that("ranking functions can handle data frames", {
  # Explicitly testing partially/fully incomplete rows
  df <- tidytable(
    year = c(2020, 2020, 2021, 2020, 2020, NA),
    month = c(3, 2, 1, 2, NA, NA)
  )

  expect_identical(row_number(df), c(3L, 1L, 4L, 2L, NA, NA))
  expect_identical(min_rank(df), c(3L, 1L, 4L, 1L, NA, NA))
  expect_identical(dense_rank(df), c(2L, 1L, 3L, 1L, NA, NA))

  expect_identical(percent_rank(df), c(2/3, 0/3, 3/3, 0/3, NA, NA))
  expect_identical(cume_dist(df), c(3/4, 2/4, 4/4, 2/4, NA, NA))

  # expect_identical(ntile(df, 2), c(2L, 1L, 2L, 1L, NA, NA))
  # expect_identical(ntile(df, 4), c(3L, 1L, 4L, 2L, NA, NA))
})
