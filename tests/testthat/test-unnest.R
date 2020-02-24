test_that("unnesting works with nested data.table", {
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

test_that("unnesting works with nested vector", {
  start_df <- data.table::data.table(
    a = 1:5,
    b = 11:15,
    c = c(rep("a", 3), rep("b", 2)),
    d = c(rep("a", 2), rep("b", 3)))

  nest_df <- start_df %>%
    dt_group_nest(c, d) %>%
    dt_mutate(vec_col = dt_map(data, ~ .x %>% dt_pull(a)))


  unnest_df <- nest_df %>%
    dt_unnest_legacy(vec_col)

  expect_named(unnest_df, c("c","d","vec_col"))
  expect_equal(unnest_df$vec_col, c(1,2,3,4,5))
})
