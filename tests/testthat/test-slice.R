test_that("works without by", {
  test_df <- data.table(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    dt_slice(1:4)

  expect_equal(sliced_df, head(test_df, 4))
})

test_that("works with by", {
  test_df <- data.table(x = c(1,2,3,4), y = c(4,5,6,7), z = c("a", "a", "a", "b"))
  sliced_df <- test_df %>%
    dt_slice(1, by = z)

  expect_equal(sliced_df$z, c("a", "b"))
  expect_equal(sliced_df$x, c(1, 4))
  expect_equal(sliced_df$y, c(4, 7))
})
