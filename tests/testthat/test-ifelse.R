test_that("dt_ifelse works", {

  df <- tidytable(x = 1:4)

  df <- df %>%
    dt_mutate(new_col = dt_ifelse(x > 2L, NA, x - 1L))

  expect_equal(df$new_col, c(0,1,NA,NA))
})

test_that("ifelse. works", {

  df <- tidytable(x = 1:4)

  df <- df %>%
    dt_mutate(new_col = ifelse.(x > 2L, NA, x - 1L))

  expect_equal(df$new_col, c(0,1,NA,NA))
})
