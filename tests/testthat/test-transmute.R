test_that("can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    transmute.(double_x = x * 2,
               double_y = y * 2)

  expect_named(df, c("double_x", "double_y"))
  expect_equal(df$double_x, c(2,4,6))
  expect_equal(df$double_y, c(2,4,6))
})

test_that("can use .by", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    transmute.(z = mean(x), .by = y)

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y][, list(y, z)]

  expect_equal(tidytable_df, datatable_df)

})
