test_that("mutate_rowwise.() adds column", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise.(sum = x + y)

  expect_equal(results_df$sum, c(5, 7, 9))
})

test_that("mutate_rowwise.() doesn't modify by reference", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise.(x = x + y)

  expect_equal(test_df$x, c(1, 2, 3))
  expect_equal(results_df$x, c(5, 7, 9))
})

test_that("c_across.() provides all columns", {
  test_df <- data.table(x = 1:3, y = 4:6)

  results_df <- test_df %>%
    mutate_rowwise.(row_mean = mean(c_across.()))
  results_df_every <- test_df %>%
    mutate_rowwise.(row_mean = mean(c_across.(everything())))

  expect_equal(results_df$row_mean, results_df_every$row_mean)
})

# test_that("c_across() works", {
#   test_df <- data.table(x = 1:3, y = 4:6)
#
#   results_df <- test_df %>%
#     mutate_rowwise.(row_mean = mean(c_across()))
#   results_df_every <- test_df %>%
#     mutate_rowwise.(row_mean = mean(c_across(everything())))
#
#   expect_equal(results_df$row_mean, results_df_every$row_mean)
# })

test_that("c_across.(cols = where(is.numeric)) provides numeric columns", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise.(pasted = mean(c_across.(cols = where(is.numeric))))


  expect_equal(results_df$pasted, c(2.5, 3.5, 4.5))
})

test_that("c_across.(!) negates column", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_rowwise.(pasted = mean(c_across.(!z)))


  expect_equal(results_df$pasted,  c(2.5, 3.5, 4.5))
})

test_that("c_across.() can only be used inside mutate_rowwise.()", {
  test_df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))


  expect_error(test_df %>%
    mutate.(pasted = paste0(c_across.(!z))))
})

test_that("c_across.() works with space named columns", {
  test_df <- data.table(`x y`=1:3, `x z`=1, y=3)
  resultdf <- test_df %>%
    mutate_rowwise.(sum = sum(c_across.(contains(" "))))

  expect_equal(resultdf$sum, c(2, 3, 4))
})
