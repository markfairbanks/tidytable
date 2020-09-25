test_df <- tidytable(a = c("a", "b", "a"), b = 3:1)

test_that("ascending order works", {

  across_df <- arrange_across.(test_df)

  check_df <- arrange.(test_df, a, b)

  expect_equal(across_df, check_df)

})

test_that("descending order works", {

  across_df <- arrange_across.(test_df, everything(), desc.)

  check_df <- arrange.(test_df, -a, -b)

  expect_equal(across_df, check_df)
})
