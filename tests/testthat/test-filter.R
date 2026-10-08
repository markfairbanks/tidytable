test_that("can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter(x <= 4)

  expect_equal(df$x, 1:4)

  df <- df %>%
    filter_out(x <= 2)

  expect_equal(df$x, 3:4)
})

test_that("can filter a data.frame", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter(x <= 4)

  expect_equal(df$x, 1:4)

  df <- df %>%
    filter_out(x <= 2)

  expect_equal(df$x, 3:4)
})

test_that("filter leaves only TRUE (no NA)", {
  df <- data.frame(x = c(1, NA, Inf))

  df <- df %>%
    filter(x == 1)

  expect_equal(df$x, 1)
})

test_that("filter_out leaves only non-TRUE (including NA)", {
  df <- data.frame(x = c(1, NA, Inf))

  df <- df %>%
    filter_out(x == 1)

  expect_equal(df$x, c(NA, Inf))
})

test_that("filter leaves input unchanged when no conditions are provided", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter()

  expect_equal(df$x, 1:10)
})

test_that("filter_out returns 0 row dt when no conditions are provided", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter_out()

  expect_equal(df$x, integer(0))
})

test_that("filter returns empty when condition has no TRUE values", {
  df <- data.frame(x = 1:10)

  df1 <- df %>%
    filter(x > 10)

  expect_equal(df1$x, integer(0))
})

test_that("filter_out returns all rows when condition has no TRUE values", {
  df <- data.frame(x = 1:10)

  df2 <- df %>%
    filter_out(x > 10)

  expect_equal(df2$x, 1:10)
})

test_that("can filter multiple conditions with commas", {
  df <- data.table(x = 1:10, y = 1:10)

  df1 <- df %>%
    filter(x <= 4, y < 3)

  expect_equal(df1$x, 1:2)
  expect_equal(df1$y, 1:2)

  df2 <- df %>%
    filter_out(x <= 2, y < 3)

  expect_equal(df2$x, 3:10)
  expect_equal(df2$y, 3:10)
})

test_that("can filter with |", {
  df <- data.table(x = 1:10, y = 1:10)

  df1 <- df %>%
    filter(x <= 4 | y < 3)

  expect_equal(df1$x, 1:4)
  expect_equal(df1$y, 1:4)

  df2 <- df %>%
    filter_out(x <= 4 | y < 3)

  expect_equal(df2$x, 5:10)
  expect_equal(df2$y, 5:10)
})

test_that("filter works with '.by'", {
  df <- data.table(x = c(1, 1, 2, 2), y = c("a", "a", "a", "b"))

  df1 <- df %>%
    filter(x == mean(x), .by = y)

  expect_named(df1, c("x", "y"))
  expect_equal(df1$x, 2)
  expect_equal(df1$y, "b")

  df2 <- df %>%
    filter_out(x == mean(x), .by = y)

  expect_named(df2, c("x", "y"))
  expect_equal(df2$x, c(1, 1, 2))
  expect_equal(df2$y, c("a", "a", "a"))
})

test_that("filter works with '.by' & multiple conditions & .N", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  result_df <- df %>%
    filter(x <= mean(x), x < .N, .by = y)

  expect_named(result_df, c("x", "y"))
  expect_equal(result_df$x, c(1))
  expect_equal(result_df$y, c("a"))

  result_df <- df %>%
    filter_out(x <= mean(x), x < .N, .by = y)

  expect_named(result_df, c("x", "y"))
  expect_equal(result_df$x, c(2, 3))
  expect_equal(result_df$y, c("a", "b"))
})

test_that("recognizes other args in custom functions & works with quosures", {
  filter_val <- function(.df, filter_col, val) {
    filter_col <- enquo(filter_col)

    .df %>%
      filter(!!filter_col == val)
  }

  test_df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  result_df <- test_df %>%
    filter_val(x, 1)

  expect_equal(result_df$x, c(1))
  expect_equal(result_df$y, c("a"))

  filter_out_val <- function(.df, filter_col, val) {
    filter_col <- enquo(filter_col)

    .df %>%
      filter_out(!!filter_col == val)
  }

  result_df <- test_df %>%
    filter_out_val(x, 1)

  expect_equal(result_df$x, c(2, 3))
  expect_equal(result_df$y, c("a", "b"))
})

test_that("works with map2() in nested data.tables", {
  filter_list <- list(1, 2, 3)
  test_df <- data.table(x = 1:3)
  test_list <- list(
    data.table(x = 1:3),
    data.table(x = 1:5),
    data.table(x = 1:10)
  )

  result_list1 <- map2(test_list, filter_list, ~ filter(.x, x == .y))

  expect_equal(result_list1[[1]]$x, c(1))
  expect_equal(result_list1[[2]]$x, c(2))
  expect_equal(result_list1[[3]]$x, c(3))

  result_list2 <- map2(test_list, filter_list, ~ .x %>% filter(x == !!.y))

  expect_equal(result_list2[[1]]$x, c(1))
  expect_equal(result_list2[[2]]$x, c(2))
  expect_equal(result_list2[[3]]$x, c(3))

  result_list1 <- map2(test_list, filter_list, ~ filter_out(.x, x == .y))

  expect_equal(result_list1[[1]]$x, c(2, 3))
  expect_equal(result_list1[[2]]$x, c(1, 3, 4, 5))
  expect_equal(result_list1[[3]]$x, c(1, 2, 4, 5, 6, 7, 8, 9, 10))

  result_list2 <- map2(test_list, filter_list, ~ .x %>% filter_out(x == !!.y))

  expect_equal(result_list2[[1]]$x, c(2, 3))
  expect_equal(result_list2[[2]]$x, c(1, 3, 4, 5))
  expect_equal(result_list2[[3]]$x, c(1, 2, 4, 5, 6, 7, 8, 9, 10))
})

test_that("properly passes quosure environment", {
  val_list <- list(2, 3)
  test_df <- data.table(x = 1:3)

  dt_filter <- function(dt, conditions) {
    conditions <- enquo(conditions)
    filter(dt, !!conditions)
  }

  result_list <- lapply(val_list, function(val) dt_filter(test_df, x < val))

  expect_equal(result_list[[1]], tidytable(x = 1))
  expect_equal(result_list[[2]], tidytable(x = 1:2))

  dt_filter_out <- function(dt, conditions) {
    conditions <- enquo(conditions)
    filter_out(dt, !!conditions)
  }

  result_list <- lapply(val_list, function(val) dt_filter_out(test_df, x < val))

  expect_equal(result_list[[1]], tidytable(x = c(2, 3)))
  expect_equal(result_list[[2]], tidytable(x = 3))
})

test_that("errors on named inputs", {
  df <- data.table(x = 1:5)

  expect_error(filter(.df, x = 4))
  expect_error(filter_out(.df, x = x == 4))
})

test_that("can use .data and .env", {
  df <- data.table(x = 1:5)

  x <- 3

  df1 <- df %>%
    filter(.data[["x"]] == .env$x)

  expect_equal(df1$x, 3)

  df2 <- df %>%
    filter_out(.data[["x"]] == .env$x)

  expect_equal(df2$x, c(1, 2, 4, 5))
})

test_that("can use .data and .env with .by", {
  df <- data.table(x = 1:6, y = vec_rep_each(c("a", "b"), 3))

  x <- 3

  df1 <- df %>%
    filter(.data[["x"]] == .env$x, .by = y)

  expect_equal(df1$x, 3)

  df2 <- df %>%
    filter_out(.data[["x"]] == .env$x, .by = y)

  expect_equal(df2$x, c(1, 2, 4, 5, 6))
})

test_that("works on a grouped_tt", {
  df <- data.table(x = c(1, 1, 2, 2), y = c("a", "a", "a", "b"))

  res <- df %>%
    group_by(y) %>%
    filter(x == mean(x))

  expect_named(res, c("x", "y"))
  expect_equal(res$x, 2)
  expect_equal(res$y, "b")
  expect_equal(group_vars(res), "y")
  expect_true(is_grouped_df(res))

  res2 <- df %>%
    group_by(y) %>%
    filter_out(x == mean(x))

  expect_named(res2, c("x", "y"))
  expect_equal(res2$x, c(1, 1, 2))
  expect_equal(res2$y, c("a", "a", "a"))
  expect_equal(group_vars(res2), "y")
  expect_true(is_grouped_df(res2))
})

test_that("properly handles NA comparisons when `.by` is used, #812", {
  df <- data.table(x = c(1, NA), y = c("a", "b"))

  res <- df %>%
    filter(x == 1, .by = y)

  expect_named(res, c("x", "y"))
  expect_equal(res$x, 1)
  expect_equal(res$y, "a")

  res2 <- df %>%
    filter_out(x == 1, .by = y)

  expect_named(res2, c("x", "y"))
  expect_equal(res2$x, NA_real_)
  expect_equal(res2$y, "b")
})
