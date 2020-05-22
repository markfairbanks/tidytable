test_that("dt_ unnesting works with nested data.table", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    dt_group_nest(c, d)

  unnest_df <- nest_df %>%
    dt_unnest_legacy(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})

test_that("unnesting works with nested data.table", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d)

  unnest_df <- nest_df %>%
    unnest.(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})

test_that("unnesting works with multiple columns", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(pulled_vec = map.(data, ~ pull.(.x, a)))

  unnest_df <- nest_df %>%
    unnest.(data, pulled_vec)

  expect_named(unnest_df, c("c","d","a","b", "pulled_vec"))
  expect_equal(unnest_df$a, start_df$a)
  expect_equal(unnest_df$pulled_vec, start_df$a)
})

test_that("automatically unnests all list_cols", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(pulled_vec = map.(data, ~ pull.(.x, a)))

  unnest_df <- nest_df %>%
    unnest.()

  expect_named(unnest_df, c("c","d","a","b","pulled_vec"))
  expect_equal(unnest_df$a, start_df$a)
  expect_equal(unnest_df$pulled_vec, start_df$a)
})

test_that("unnesting works with nested vector", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(vec_col = map.(data, ~ .x %>% pull.(a)))


  unnest_df <- nest_df %>%
    unnest.(vec_col)

  expect_named(unnest_df, c("c","d","vec_col"))
  expect_equal(unnest_df$vec_col, c(1,2,3,4,5))
})

test_that("unnesting works with nested data.frames", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(data = map.(data, as.data.frame))

  unnest_df <- nest_df %>%
    unnest.(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})



test_that("unnesting works with different ordered/different # of columns", {
  df1 <- data.table(a = "a", b = 1)
  df2 <- data.table(b = 1:3, a = rep("a", 3), c = 1:3)

  nested_df <- data.table(id = 1:2,
                          list_col = list(df1, df2))

  unnest_df <- nested_df %>%
    unnest.(list_col)

  expect_named(unnest_df, c("id","a","b","c"))
  expect_equal(unnest_df$b, c(1, 1:3))
})

test_that("unnesting works with nested data.table with quosure function", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    nest_by.(c, d)

  unnest_fn <- function(.df, col) {
    unnest.(.df, {{ col }})
  }

  unnest_df <- nest_df %>%
    unnest_fn(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})
