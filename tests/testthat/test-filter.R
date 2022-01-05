test_that("can filter the data.table", {
  df <- data.table(x = 1:10)

  df <- df %>%
    filter.(x <= 4)

  expect_equal(df$x, 1:4)
})

test_that("can filter a data.frame", {
  df <- data.frame(x = 1:10)

  df <- df %>%
    filter.(x <= 4)

  expect_equal(df$x, 1:4)
})

test_that("can filter multiple conditions with commas", {
  df <- data.table(x = 1:10, y = 1:10)

  df <- df %>%
    filter.(x <= 4, y < 3)

  expect_equal(df$x, 1:2)
  expect_equal(df$y, 1:2)
})

test_that("can filter with |", {
  df <- data.table(x = 1:10, y = 1:10)

  df <- df %>%
    filter.(x <= 4 | y < 3)

  expect_equal(df$x, 1:4)
  expect_equal(df$y, 1:4)
})

test_that("filter works with '.by'", {
  df <- data.table(x = c(1, 1, 2, 2), y = c("a", "a", "a", "b"))

  df <- df %>%
    filter.(x == mean(x), .by = y)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, 2)
  expect_equal(df$y, "b")
})

test_that("filter works with '.by' & multiple conditions & .N", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  result_df <- df %>%
    filter.(x <= mean(x), x < .N, .by = y)

  expect_named(result_df, c("x", "y"))
  expect_equal(result_df$x, c(1))
  expect_equal(result_df$y, c("a"))
})

test_that("recognizes other args in custom functions & works with quosures", {

  filter_val <- function(.df, filter_col, val) {
    filter_col <- enquo(filter_col)

    .df %>%
      filter.(!!filter_col == val)
  }

  test_df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  result_df <- test_df %>%
    filter_val(x, 1)

  expect_equal(result_df$x, c(1))
  expect_equal(result_df$y, c("a"))
})

test_that("works with map2.() in nested data.tables", {
  filter_list <- list(1,2,3)
  test_df <- data.table(x = 1:3)
  test_list <- list(data.table(x = 1:3),
                    data.table(x = 1:5),
                    data.table(x = 1:10))

  result_list1 <- map2.(test_list, filter_list, ~ filter.(.x, x == .y))

  expect_equal(result_list1[[1]]$x, c(1))
  expect_equal(result_list1[[2]]$x, c(2))
  expect_equal(result_list1[[3]]$x, c(3))

  result_list2 <- map2.(test_list, filter_list, ~ .x %>% filter.(x == !!.y))

  expect_equal(result_list2[[1]]$x, c(1))
  expect_equal(result_list2[[2]]$x, c(2))
  expect_equal(result_list2[[3]]$x, c(3))
})

test_that("properly passes quosure environment", {
  val_list <- list(2, 3)
  test_df <- data.table(x = 1:3)

  dt_filter <- function(dt, conditions) {
    conditions <- enquo(conditions)
    filter.(dt, !!conditions)
  }

  result_list <- lapply(val_list, function(val) dt_filter(test_df, x < val))

  expect_equal(result_list[[1]], tidytable(x = 1))
  expect_equal(result_list[[2]], tidytable(x = 1:2))
})

test_that("errors on named inputs", {
  df <- data.table(x = 1:5)

  expect_error(filter.(.df, x = 4))
})

test_that("can use .data and .env", {
  df <- data.table(x = 1:5)

  x <- 3

  df <- df %>%
    filter.(.data[["x"]] == .env$x)

  expect_equal(df$x, 3)
})

test_that("can use .data and .env with .by", {
  df <- data.table(x = 1:6, y = vec_rep_each(c("a", "b"), 3))

  x <- 3

  df <- df %>%
    filter.(.data[["x"]] == .env$x, .by = y)

  expect_equal(df$x, 3)
})
