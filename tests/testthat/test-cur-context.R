test_that("cur_column() works", {
  df <- tidytable(x = 1:3, y = 4:6)
  res <- df %>%
    mutate(
      across(c(x, y), ~ paste0(cur_column(), .x))
    )
  expect_equal(res$x, c("x1", "x2", "x3"))
  expect_equal(res$y, c("y4", "y5", "y6"))

  # Works in an anonymous function, #699
  res <- df %>%
    mutate(
      across(c(x, y), \(.x) paste0(cur_column(), .x))
    )
  expect_equal(res$x, c("x1", "x2", "x3"))
  expect_equal(res$y, c("y4", "y5", "y6"))
})

test_that("cur_data() works", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))
  result_df <- df %>%
    summarize(data = list(cur_data()), .by = y)
  check_df <- df %>%
    nest_by(y)

  expect_equal(result_df, check_df)
})

test_that("cur_group_rows() works", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))
  df <- df %>%
    mutate(rows1 = cur_group_rows(),
           rows2 = cur_group_rows.(),
           rows_check = 1:.N,
           .by = y)

  expect_equal(df$rows1, df$rows_check)
  expect_equal(df$rows2, df$rows_check)
})


test_that("cur_group_id() works", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))
  df <- df %>%
    mutate(id1 = cur_group_id(),
            id2 = cur_group_id.(),
            id_check = .GRP,
            .by = y)

  expect_equal(df$id1, df$id_check)
  expect_equal(df$id2, df$id_check)
})
