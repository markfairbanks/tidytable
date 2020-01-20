test_that("works on numeric columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    dt_mutate(x = dt_replace_na(x, 5))

  expect_equal(replaced_df$x, c(1,2,5))
})

test_that("works on numeric columns", {
  test_df <- data.table(x = c(1, 2, NA), y = c("a", NA, "c"))
  replaced_df <- test_df %>%
    dt_mutate(y = dt_replace_na(y, "b"))

  expect_equal(replaced_df$y, c("a", "b", "c"))
})
