test_that("if_else. works with true = NA", {
  df <- tidytable(x = 1:4)

  df <- df %>%
    mutate.(new_col = if_else.(x > 2L, NA, x - 1L))

  expect_equal(df$new_col, c(0, 1, NA, NA))
})

test_that("if_else. works with false = NA", {
  df <- tidytable(x = 1:4)

  df <- df %>%
    mutate.(new_col = if_else.(x > 2L, x - 1L, NA))

  expect_equal(df$new_col, c(NA, NA, 2, 3))
})

test_that("dplyr::if_else() is converted to if_else.()", {
  # This test should pass without dplyr being loaded
  df <- tidytable(x = 1:4)

  # Unnamed arguments
  df_unnamed <- mutate.(df, new_col = if_else(x > 2L, x - 1L, NA))
  expect_equal(df_unnamed$new_col, c(NA, NA, 2, 3))

  df <- tidytable(x = c(1:3, NA))

  # Named arguments
  df_named <- mutate.(df, new_col = if_else(x > 2L, false = NA, tr = x - 1L,  missing = 3))
  expect_equal(df_named$new_col, c(NA, NA, 2, 3))
})

test_that("base::ifelse() is converted to ifelse.()", {
  df <- tidytable(x = 1:4)

  # Unnamed arguments
  df_unnamed <- mutate.(df, new_col = ifelse(x > 2L, x - 1L, NA))
  expect_equal(df_unnamed$new_col, c(NA, NA, 2, 3))

  # Named arguments
  df_named <- mutate.(df, new_col = ifelse(x > 2L, yes = x - 1L, no = NA))
  expect_equal(df_named$new_col, c(NA, NA, 2, 3))
})
