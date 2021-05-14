test_that("cur_group_rows.() works", {
  df <- data.table(x = 1:3, y = c("a", "a", "b"))
  df <- df %>%
    mutate.(rows1 = cur_group_rows.(),
            rows2 = cur_group_rows(),
            rows_check = 1:.N,
            .by = y)

  expect_equal(df$rows1, df$rows_check)
  expect_equal(df$rows2, df$rows_check)
})


test_that("cur_group_id.() works", {
  df <- data.table(x = 1:3, y = c("a", "a", "b"))
  df <- df %>%
    mutate.(id1 = cur_group_id.(),
            id2 = cur_group_id(),
            id_check = .GRP,
            .by = y)

  expect_equal(df$id1, df$id_check)
  expect_equal(df$id2, df$id_check)
})
