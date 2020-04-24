test_that("can remove variables with NULL", {
  df <- data.table(x = 1:3, y = 1:3)
  tidytable_df <- df %>% mutate.(y = NULL)
  df_check <- as_tidytable(df)

  expect_equal(tidytable_df, df_check[, 1])

  # if it doesn't exist
  expect_warning(df %>% mutate.(z = NULL))
})

test_that("can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
})

test_that("row_number.() works", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(row = row_number.(),
            row_check = 1:.N)

  expect_equal(df$row, df$row_check)
})

test_that("dt_ can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    dt_mutate(double_x = x * 2,
              double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
})

test_that("modify-by-reference doesn't occur", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y"))
})

test_that("modify-by-reference doesn't occur with single val", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate.(x = 1)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("column order is correct", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            x = 1)

  expect_named(df, c("x", "y", "double_x"))
})

test_that("can take data.frame input", {
  df <- data.frame(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
})

test_that("can use by", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), by = y)

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

test_that("can use by with enhanced selection", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), by = is.character)

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

test_that("can use by with vector", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), by = c(is.character))

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

# test_that("can use by with list", {
#   df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))
#
#   tidytable_df <- df %>%
#     mutate.(z = mean(x), by = list(is.character))
#
#   datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]
#
#   expect_equal(tidytable_df, datatable_df)
#
# })
