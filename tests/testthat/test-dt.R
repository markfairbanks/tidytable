test_that("doesn't modify by reference", {
  df <- data.table(x = 1:3, y = 1:3, z = 1:3)

  df %>%
    dt(, x := x * 2)

  df %>%
    dt(, x := 2)

  df %>%
    dt(, double_x := x * 2)

  df %>%
    dt(, x := NULL)

  df %>%
    dt(, c("x", "y") := NULL)

  expect_named(df, c("x", "y", "z"))
  expect_equal(df$x, 1:3)
})

test_that("can filter", {
  df <- data.table(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(y <= 3)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,1,1))
})

test_that("can slice", {
  df <- data.table(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(1)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, 1)
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
  expect_equal(df$b, 1:3)
  expect_equal(df$c, 1:3)
})

test_that("converts a data.frame input", {
  df <- data.frame(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(, x := x * 2)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(2,2,2,2))
})

test_that("works with outside defined column name", {
  df <- data.frame(x = 1:3, y = 1:3)

  new_col <- "new"
  out <- df %>%
    dt(, (new_col) := x * 2)

  expect_named(out, c("x", "y", "new"))
  expect_equal(out$new, c(2, 4, 6))
})

test_that("works with tidy evaluation", {
  df <- tidytable(x = 1:3, y = 1:3)

  add_val <- function(data, col, val) {
    data %>%
      dt(, {{ col }} := {{ col }} + val)
  }

  out <- df %>%
    add_val(x, 1)

  expect_named(out, c("x", "y"))
  expect_equal(out$x, 2:4)
  expect_equal(df$x, 1:3)
})

test_that("works with tidy evaluation v2", {
  df <- tidytable(x = 1:3, y = 1:3)

  col <- quo(x)

  out <- df %>%
    dt(, .(!!col := mean(!!col), y = mean(y))) %>%
    dt(, .(!!col, y)) %>%
    dt(!!col == 2) %>%
    dt(, ':='(!!col := !!col * 2, double_y = y * 2))

  expect_named(out, c("x", "y", "double_y"))
  expect_equal(out$x, 4)
  expect_equal(out$y, 2)
  expect_equal(out$double_y, 4)
  expect_equal(df$x, 1:3)
})

test_that("doesn't work on list input", {
  df <- list(x = c(4,3,9,7), y = 1:4)
  expect_error(df %>% dt(, x := 1))
})
