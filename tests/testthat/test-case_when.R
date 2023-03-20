test_that("works", {
  x <- 1:5

  case_x <- case_when(
    x < 3 ~ 1,
    x < 4 ~ 2,
    TRUE ~ 3
  )

  expect_equal(case_x, c(1, 1, 2, 3, 3))
})

test_that("isn't tripped up by NA results v1", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate(check = case_when(is.na(x) ~ 1,
                             x < 2 ~  2,
                             TRUE ~ 0))

  expect_equal(case_df$check, c(2, 1, 2, 0))
})

test_that("isn't tripped up by NA results v2", {
  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate(check = case_when(x < 2 ~ 2,
                             is.na(x) ~ 1,
                             TRUE ~ 0))

  expect_equal(case_df$check, c(2, 1, 2, 0))
})

test_that("lower conditions don't overwrite prior conditions", {
  x <- 1:5

  new_x <- case_when(x < 2 ~ 1,
                     x < 4 ~ 2,
                     .default = 3)

  expect_equal(new_x, c(1, 2, 2, 3, 3))
})

test_that("multiple NAs can be used as inputs", {
  x <- 1:5

  new_x <- case_when(x < 2 ~ 1,
                     x < 4 ~ 2,
                     x < 5 ~ NA,
                     .default = NA)

  expect_equal(new_x, c(1, 2, 2, NA, NA))
})

test_that("passes through `.ptype` correctly", {
  res <- case_when(TRUE ~ 1, .ptype = integer())
  expect_identical(res, 1L)
})

test_that("passes through `.size` correctly", {
  res <- case_when(TRUE ~ 1, .size = 2)
  expect_identical(res, c(1, 1))
})

test_that("use `.default` to find common ptype", {
  df <- tidytable(x = 1:3, y = c("a", "b", "c"))

  res <- df %>%
    mutate(case = case_when(y == "a" ~ NA,
                            .default = x)) %>%
    pull(case)

  expect_equal(res, c(NA, 2, 3))
})

test_that("case_when. works", {
  x <- 1:5

  case_x <- case_when.(x < 3 ~ 1,
                       x < 4 ~ 2,
                       TRUE ~ 3) %>%
    suppressWarnings()

  expect_equal(case_x, c(1, 1, 2, 3, 3))
})
