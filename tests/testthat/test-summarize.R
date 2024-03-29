test_that("can do group aggregation with .by", {
  df <- tidytable(x = 3:1, y = c("b", "a", "a"))

  result_df <- df %>%
    summarize(avg_x = mean(x), .by = y)

  expect_named(result_df, c("y", "avg_x"))
  expect_equal(result_df$avg_x, c(1.5, 3))
  expect_false(data.table::haskey(result_df))
})

test_that("n() works", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  result_df <- df %>%
    summarize(count = n(), .by = y)

  expect_equal(result_df$count, c(3, 1))
})

test_that(".GRP works", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  result_df <- df %>%
    summarize(group_id = .GRP, .by = y)

  expect_equal(result_df$group_id, c(1, 2))
})

test_that("can do group aggregation with no by", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  result_df <- df %>%
    summarize(avg_x = mean(x))

  expect_named(result_df, "avg_x")
  expect_equal(result_df$avg_x, 2)
})

test_that("can use .sort", {
  test_df <- tidytable(x = 1:3, y = c("b", "a", "a"), z = c("b", "a", "a"))

  result_df <- test_df %>%
    summarize(avg_x = mean(x),
              .by = c(y, z),
              .sort = FALSE)

  expect_named(result_df, c("y", "z", "avg_x"))
  expect_equal(result_df$y, c("b", "a"))
})

test_that("works with data.frame input", {
  df <- vctrs::data_frame(x = 3:1, y = c("b", "a", "a"))

  result_df <- df %>%
    summarize(avg_x = mean(x), .by = y)

  expect_named(result_df, c("y", "avg_x"))
  expect_equal(result_df$avg_x, c(1.5, 3))
})

test_that("can make a function with quosures", {
  df <- tidytable(x = 1:4, y = c("a", "a", "a", "b"))

  summarize_fn <- function(.df, col, val) {
    .df %>%
      summarize(avg_x = mean({{ col }}) + val,
                .by = y)
  }

  result_df <- df %>%
    summarize_fn(x, 1)

  expect_equal(result_df$avg_x, c(3, 5))
})

test_that("can use .data and .env", {
  df <- tidytable(x = 1:3)

  x <- 1

  result_df <- df %>%
    summarize(check = mean(.data$x) + .env$x)

  expect_equal(result_df$check, 3)
})

test_that("can use .data and .env with .by", {
  df <- tidytable(x = 1:4, y = c("a","a","a","b"))

  x <- 1

  result_df <- df %>%
    summarize(check = mean(.data$x) + .env$x,
              .by = y)

  expect_equal(result_df$check, c(3, 5))
})

test_that("empty dots with .by returns distinct, #379", {
  df <- tidytable(x = c("a", "a", "b"), y = c("a", "a", "b"))

  result_df <- df %>%
    summarize(.by = c(x, y))

  check_df <- tidytable(x = c("a", "b"), y = c("a", "b"))

  expect_equal(result_df, check_df)
})

test_that("works with grouped_tt", {
  df <- tidytable(x = c("a", "a", "b"), y = c("a", "a", "b"), z = 1:3)

  tidytable_df <- df %>%
    group_by(x, y) %>%
    summarize(avg_z = mean(z))

  check_df <- tidytable(x = c("a", "b"), y = c("a", "b"), avg_z = c(1.5, 3)) %>%
    group_by(x)

  expect_equal(tidytable_df, check_df)
})

test_that("works with grouped_tt & .groups", {
  df <- tidytable(x = c("a", "a", "b"), y = c("a", "a", "b"), z = 1:3)

  tidytable_df <- df %>%
    group_by(x, y) %>%
    summarize(avg_z = mean(z),
               .groups = "drop")

  check_df <- tidytable(x = c("a", "b"), y = c("a", "b"), avg_z = c(1.5, 3))

  expect_equal(tidytable_df, check_df)

  tidytable_df <- df %>%
    group_by(x, y) %>%
    summarize(avg_z = mean(z),
               .groups = "keep")

  check_df <- tidytable(x = c("a", "b"), y = c("a", "b"), avg_z = c(1.5, 3)) %>%
    group_by(x, y)

  expect_equal(tidytable_df, check_df)
})

test_that("can unpack data frame inputs, #576", {
  df <- tidytable(group = c("a", "a", "b"), val = 1:3)

  fun <- function(x) {
    data.frame(mean = mean(x), max = max(x))
  }

  res <- df %>%
    summarize(fun(val), .by = group, .unpack = TRUE)

  expect_named(res, c("group", "mean", "max"))
  expect_equal(res$mean, c(1.5, 3))

  # Errors when `.unpack = FALSE`
  expect_error(summarize(df, fun(val), .by = group))
})

test_that("errors on rowwise_tt", {
  df <- rowwise(tidytable(x = 1:3, y = 1:3))
  expect_error(summarize(df, avg_x = mean(x)))
})
