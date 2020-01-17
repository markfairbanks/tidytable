test_that("dt_case works", {

  set.seed(843)
  x <- rnorm(1e5)

  cased <-
    dt_case(
      x < median(x), "low",
      x >= median(x), "high",
      is.na(x), "other"
    )

  cased2 <- dt_case(x > median(x), 1,
                    default = x)

  expect_named(table(cased), c("high", "low"))
  expect_error(dt_case(x < median(x), "three",
                            default = x))
  expect_equal(head(cased),
               c("low","low","low","low","low","high"))

})
