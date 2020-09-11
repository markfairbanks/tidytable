test_that("ifelse. works with true = NA", {

  df <- tidytable(x = 1:4)

  df <- df %>%
    mutate.(new_col = ifelse.(x > 2L, NA, x - 1L))

  expect_equal(df$new_col, c(0,1,NA,NA))
})

test_that("ifelse. works with false = NA", {

  df <- tidytable(x = 1:4)

  df <- df %>%
    mutate.(new_col = ifelse.(x > 2L, x - 1L, NA))

  expect_equal(df$new_col, c(NA,NA,2,3))
})
