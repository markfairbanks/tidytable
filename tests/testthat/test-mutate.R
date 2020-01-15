test_that("can remove variables with NULL", {
  df <- data.table(x = 1:3, y = 1:3)

  expect_equal(df %>% dt_mutate(y = NULL), df[, 1])

  # if it doesn't exist
  expect_warning(df %>% dt_mutate(z = NULL))
})

test_that("can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    dt_mutate(double_x = x * 2,
              double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
})

test_that("can use by", {
  df <- data.table(x = 1:5, y = c(rep("a", 4), "b"))

  tidyfast_df <- copy(df) %>%
    dt_mutate(z = mean(x), by = y)

  datatable_df <- copy(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidyfast_df, datatable_df)

})
