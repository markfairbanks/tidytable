test_that("expect error when empty", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))


  expect_error(dt_pull(test_df), "var must be supplied")
})

test_that("returns a vec", {
  test_df <- data.table(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    dt_pull(y)

  expect_equal(test_df$y, result_vec)
})

test_that("returns a vec with data.frame input", {
  test_df <- data.frame(x = c(1,2,3), y = c(4,5,6))
  result_vec <- test_df %>%
    dt_pull(y)

  expect_equal(test_df$y, result_vec)
})
