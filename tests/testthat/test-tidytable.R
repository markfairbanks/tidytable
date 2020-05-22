test_that("can construct using quosures", {
  create_dt <- function(name, val) {
    tidytable({{name}} := {{val}})
  }

  df <- create_dt(stuff, 1:3)

  expect_named(df, c("stuff"))
  expect_equal(df$stuff, 1:3)
})
