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
