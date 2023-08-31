test_that("can nest all data & can specify name", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  result_df <- df %>%
    nest(data = everything())

  expect_named(result_df, "data")
  expect_equal(result_df, nest(df))

  result_df <- df %>%
    nest(stuff = everything())

  expect_named(result_df, "stuff")
  expect_equal(nrow(result_df), 1)
})

test_that("can specify columns in `...`", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  result_df <- df %>%
    nest(stuff = c(a, b))

  expect_named(result_df, c("c", "stuff"))
  expect_true(is.list(result_df$stuff))
  expect_equal(nrow(result_df), 2)
})

test_that(".names_sep works", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  result_df <- df %>%
    nest(data = c(a, b), .names_sep = "_") %>%
    unnest(data)

  expect_named(result_df, c("c", "data_a", "data_b"))
})

test_that("can nest by group with quosure function", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  nest_fn <- function(.df, cols) {
    nest(.df, data = {{ cols }})
  }

  result_df <- df %>%
    nest_fn(cols = c(a, b))

  expect_named(result_df, c("c", "data"))
})

test_that("works on grouped_tt", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  res <- df %>%
    group_by(c) %>%
    nest()

  expect_named(res, c("c", "data"))
  expect_true(is.list(res$data))
  expect_equal(group_vars(res), "c")
  expect_true(is_grouped_df(res))
})

test_that("works with .by", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  res <- df %>%
    nest(.by = c, .key = "nest_data")

  expect_named(res, c("c", "nest_data"))
  expect_true(is.list(res$nest_data))
})

test_that("works with .by and `...`", {
  df <- data.table(a = 1:3,
                   b = 4:6,
                   c = c("a", "a", "b"))

  res <- df %>%
    nest(nest_data = a, .by = c)

  expect_named(res, c("c", "nest_data"))
  expect_true(is.list(res$nest_data))
  expect_named(res$nest_data[[1]], "a")
})
