test_that("can nest all data & can specify name", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest(data = everything())

  expect_named(result_df, c("data"))

  result_df <- test_df %>%
    nest(stuff = everything())

  expect_named(result_df, c("stuff"))
  expect_equal(nrow(result_df), 1)
})

test_that("nest. works", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest.(data = everything())

  expect_named(result_df, c("data"))
})

test_that("can nest certain columns", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest(data = c(a, b))

  expect_named(result_df, c("c", "data"))

  result_df <- test_df %>%
    nest(stuff = c(a, b))

  expect_named(result_df, c("c", "stuff"))
  expect_equal(class(result_df$stuff), "list")
  expect_equal(nrow(result_df), 2)
})

test_that(".names_sep works", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  result_df <- test_df %>%
    nest(data = c(a, b), .names_sep = "_") %>%
    unnest(data)

  expect_named(result_df, c("c", "data_a", "data_b"))
})

test_that("can nest by group with quosure function", {
  test_df <- data.table(a = 1:3,
                        b = 4:6,
                        c = c("a", "a", "b"))

  nest_fn <- function(.df, cols) {
    nest(.df, data = {{ cols }})
  }

  result_df <- test_df %>%
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
