test_that("dt() doesn't modify by reference", {
  df <- data.table(x = c(1,1,1,1), y = 1:4)

  df %>%
    dt(, x := x * 2)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,1,1,1))
})

test_that("dt() converts a data.frame input", {
  df <- data.frame(x = c(1,1,1,1), y = 1:4)

  df <- df %>%
    dt(, x := x * 2)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(2,2,2,2))
})

test_that("dt() doesn't work on list input", {
  df <- list(x = c(4,3,9,7), y = 1:4)

  expect_error(df %>% dt(, x := 1))
})
