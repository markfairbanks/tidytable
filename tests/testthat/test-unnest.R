start_df <- tidytable(
  a = 1:5,
  b = 11:15,
  c = c(rep("a", 3), rep("b", 2)),
  d = c(rep("a", 2), rep("b", 3)))

test_that("unnesting works with nested data.table", {

  nest_df <- start_df %>%
    nest_by.(c, d)

  unnest_df <- nest_df %>%
    unnest.(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})

test_that("names_sep works", {

  nest_df <- start_df %>%
    nest_by.(c, d)

  unnest_df <- nest_df %>%
    unnest.(data, names_sep = "_")

  expect_named(unnest_df, c("c","d","data_a","data_b"))
  expect_equal(unnest_df$data_a, start_df$a)
})

test_that("unnesting works with multiple columns", {

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

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(vec_col = map.(data, ~ .x %>% pull.(a)))


  unnest_df <- nest_df %>%
    unnest.(vec_col)

  expect_named(unnest_df, c("c","d","vec_col"))
  expect_equal(unnest_df$vec_col, c(1,2,3,4,5))
})

test_that("unnesting works with nested data.frames", {

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(data = map.(data, as.data.frame))

  unnest_df <- nest_df %>%
    unnest.(data)

  expect_named(unnest_df, c("c","d","a","b"))
  expect_equal(unnest_df$a, start_df$a)
})

test_that("unnesting works with nested matrices", {

  nest_df <- start_df %>%
    nest_by.(c, d) %>%
    mutate.(data = map.(data, as.matrix))

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

test_that("unnesting works with nested data.table with quosure function", {
  data_size <- 3
  list_df <- data.table(test = 1:data_size)
  test_df <- data.table(x = 1:data_size,
                        y = replicate(data_size, list_df, simplify = FALSE)) %>%
    mutate.(z = y)

  result_df <- test_df %>%
    unnest.(y, .drop = FALSE)

  expect_named(result_df, c("x","z","test"))
  expect_true(is.list(result_df$z))
  expect_equal(result_df$test, rep(1:3, 3))
})

test_that("works when the only column is a list column", {
  list_df <- tidytable(x = 1:3)
  test_df <- tidytable(list_col = list(list_df, list_df, list_df))

  result_df <- unnest.(test_df, list_col)

  expect_equal(result_df$x, rep(1:3, 3))
})
