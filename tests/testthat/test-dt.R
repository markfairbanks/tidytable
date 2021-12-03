test_that("dt() doesn't modify by reference", {
  df <- data.table(x = c(1,1,1,1), y = 1:4)

  df %>%
    dt(, x := x * 2)

  df %>%
    dt(, x := 2)

  df %>%
    dt(, double_x := 2)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,1,1,1))
})

test_that("can do a simple filter", {
  df <- data.table(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(y <= 3)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,1,1))
})

test_that("works with lapply", {
  df <- tidytable(a = c("a", "a", "a"),
                  b = 1:3,
                  c = 1:3)

  check_df <- df %>%
    dt(, c("b", "c") := lapply(.SD, mean), .SDcols = c("b", "c"))

  expect_named(check_df, c("a", "b", "c"))
  expect_equal(check_df$b, c(2,2,2))
  expect_equal(check_df$c, c(2,2,2))
})

test_that("dt() converts a data.frame input", {
  df <- data.frame(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(, x := x * 2)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(2,2,2,2))
})

test_that("dt() doesn't work on list input", {
  df <- list(x = c(4,3,9,7), y = 1:4)

  expect_error(df %>% dt(, x := 1))
})
