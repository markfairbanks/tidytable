test_that("case. works", {

  set.seed(843)
  x <- rnorm(1e5)

  cased <- case.(
      x < median(x), "low",
      x >= median(x), "high",
      is.na(x), "other"
    )


  expect_named(table(cased), c("high", "low"))
  expect_error(case.(x < median(x), "three",
                       default = x))
  expect_equal(head(cased),
               c("low","low","low","low","low","high"))

})

test_that("dt_case works", {

  set.seed(843)
  x <- rnorm(1e5)

  cased <- dt_case(
      x < median(x), "low",
      x >= median(x), "high",
      is.na(x), "other"
    )

  expect_named(table(cased), c("high", "low"))
  expect_error(dt_case(x < median(x), "three",
                            default = x))
  expect_equal(head(cased),
               c("low","low","low","low","low","high"))

})

test_that("case. isn't tripped up by NA results v1", {

  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate.(check = case.(is.na(x), 1,
                          x < 2,  2,
                          default = 0))

  expect_equal(case_df$check, c(2,1,2,0))
})

test_that("case. isn't tripped up by NA results v2", {

  test_df <- tidytable(x = c(1, NA, 1, 2))

  case_df <- test_df %>%
    mutate.(check = case.(x < 2, 2,
                          is.na(x), 1,
                          default = 0))

  expect_equal(case_df$check, c(2,1,2,0))
})
