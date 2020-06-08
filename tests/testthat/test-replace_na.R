test_that("dt_ works on numeric columns & is deprecated", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    mutate.(x = dt_replace_na(x, 5))

  expect_deprecated(mutate.(test_df, x = dt_replace_na(x, 5)))
  expect_equal(replaced_df$x, c(1,2,5))
})

test_that("works on numeric columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    mutate.(x = replace_na.(x, 5))

  expect_equal(replaced_df$x, c(1,2,5))
})

test_that("works on character columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    mutate.(y = replace_na.(y, "b"))

  expect_equal(replaced_df$y, c("a", "b", "c"))
})
