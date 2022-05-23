test_that("can construct a tidytable", {

  df <- tidytable(stuff = 1:3)

  expect_named(df, c("stuff"))
  expect_equal(df$stuff, 1:3)
  expect_true(inherits(df, "tidytable"))
})

test_that("can construct using quosures", {
  create_dt <- function(name, val) {
    tidytable({{ name }} := val)
  }

  df <- create_dt(stuff, 1:3)

  expect_named(df, c("stuff"))
  expect_equal(df$stuff, 1:3)
})

test_that("can create an empty tidytable", {
  df <- tidytable()

  expect_equal(names(df), character(0))
  expect_equal(nrow(df), 0)
})

test_that("can splice quosures", {
  vals <- quos(x = 1, y = 2)
  df <- tidytable(!!!vals)

  expect_equal(df, tidytable(x = 1, y = 2))
})

test_that("works with data frame inputs, #471", {
  df <- tidytable(x = 1, y = 2)
  expect_equal(tidytable(df), tidytable(x = 1, y = 2))

  # Errors on named data frame inputs
  expect_error(tidytable(df = df))
})
