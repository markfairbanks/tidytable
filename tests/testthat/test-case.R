test_that("works", {
  x <- 1:5

  case_x <- case(x < 3, 1,
                 x < 4, 2,
                 TRUE, 3)

  expect_equal(case_x, c(1,1,2,3,3))
})

test_that("isn't tripped up by NA results v1", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate(check = case(is.na(x), 1,
                        x < 2,  2,
                        default = 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("isn't tripped up by NA results v2", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate(check = case(x < 2, 2,
                        is.na(x), 1,
                        default = 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("lower conditions don't overwrite prior conditions", {
  x <- 1:10

  new_x <- case(x < 5, 1,
                x < 9, 2,
                default = 3)

  expect_equal(new_x, c(1,1,1,1,2,2,2,2,3,3))
})

test_that("multiple NAs can be used as inputs", {
  x <- 1:10

  new_x <- case(x < 3, 1,
                x < 6, 2,
                x < 10, NA,
                default = NA)

  expect_equal(new_x, c(1,1,2,2,2,NA,NA,NA,NA,NA))
})

test_that("works with vector default", {
  x <- 1:5

  case_x <- case(x < 3, 1,
                 x < 4, 2,
                 default = rep(3, 5))

  expect_equal(case_x, c(1,1,2,3,3))
})
