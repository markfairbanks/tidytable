test_that("dt_ can add and drop columns", {
  df <- data.table(x = 1, y = 2, z = 3)
  df <- df %>%
    dt_select(x, y, -y)

  expect_named(df, c("x"))
})

test_that("can add and drop columns", {
  df <- data.table(x = 1, y = 2, z = 3)
  df <- df %>%
    select.(x, y, -y)

  expect_named(df, c("x"))
})

test_that("can add and drop columns w/ data.frame", {
  df <- data.frame(x = 1, y = 2, z = 3)
  df <- df %>%
    select.(x, y, -y)

  expect_named(df, c("x"))
})

test_that("can select a range of columns", {
  df <- data.table(x = 1, y = 2, z = 3)
  df <- df %>%
    select.(x:z)

  expect_named(df, c("x", "y", "z"))
})

test_that("can use select helpers", {
  df <- data.table(x_start = 1, end_x = 1,
                   y_start = 1, end_y = 1,
                   stuff = 1)
  df <- df %>%
    select.(starts_with.("x"), ends_with.("y"), contains.("tuf"))

  expect_named(df, c("x_start", "end_y", "stuff"))
})

test_that("works with predicates", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    select.(is.numeric)

  expect_named(df, c("x", "y"))
})
