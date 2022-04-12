test_that("doesn't modify by reference", {
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

# test_that("works with tidy evaluation", {
#   df <- tidytable(x = 1:3, y = 1:3)
#
#   add_val <- function(data, col, val) {
#     data %>%
#       dt(, {{ col }} := {{ col }} + val)
#   }
#
#   out <- df %>%
#     add_val(x, 1)
#
#   expect_named(out, c("x", "y"))
#   expect_equal(out$x, 2:4)
#   expect_equal(df$x, 1:3)
# })
#
# test_that("works with tidy evaluation v2", {
#   df <- tidytable(x = 1:3, y = 1:3)
#
#   add_val <- function(data, col, val) {
#     data %>%
#       dt(, {{ col }} := {{ col }} + val)
#   }
#
#   out <- df %>%
#     add_val(x, 1)
#
#   expect_named(out, c("x", "y"))
#   expect_equal(out$x, 2:4)
#   expect_equal(df$x, 1:3)
# })

test_that("dt() doesn't work on list input", {
  df <- list(x = c(4,3,9,7), y = 1:4)
  expect_error(df %>% dt(, x := 1))
})
