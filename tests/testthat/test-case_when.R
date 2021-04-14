test_that("case_when. works", {
  x <- 1:5

  case_x <- case_when.(
    x < 3 ~ 1,
    x < 4 ~ 2,
    TRUE ~ 3
  )

  expect_equal(case_x, c(1,1,2,3,3))
})

test_that("case_when. isn't tripped up by NA results v1", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate.(check = case_when.(is.na(x) ~ 1,
                               x < 2 ~  2,
                               TRUE ~ 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("case_when. isn't tripped up by NA results v2", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate.(check = case_when.(x < 2 ~ 2,
                               is.na(x) ~ 1,
                               TRUE ~ 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("dplyr::case_when() is replaced by tidytable::case_when.()", {
  # Test should pass even without dplyr loaded
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate.(check = case_when(x < 2 ~ 2,
                              is.na(x) ~ 1,
                              TRUE ~ 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("lower conditions don't overwrite prior conditions", {
  x <- 1:10

  new_x <- case_when.(x < 5 ~ 1,
                      x < 9 ~ 2,
                      TRUE ~ 3)

  expect_equal(new_x, c(1,1,1,1,2,2,2,2,3,3))
})

test_that("multiple NAs can be used as inputs", {
  x <- 1:10

  new_x <- case_when.(x < 3 ~ 1,
                      x < 6 ~ 2,
                      x < 10 ~ NA,
                      TRUE ~ NA)

  expect_equal(new_x, c(1,1,2,2,2,NA,NA,NA,NA,NA))
})
