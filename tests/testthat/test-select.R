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
    select.(starts_with("x"), ends_with("y"), contains("tuf"))

  expect_named(df, c("x_start", "end_y", "stuff"))
})

test_that("works with predicates", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    select.(where(is.numeric))

  expect_named(df, c("x", "y"))
})

test_that("can rename columns & doesn't modify by reference", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  new_df <- df %>%
    select.(new = x, y, stuff = z)

  expect_named(df, c("x", "y", "z"))
  expect_named(new_df, c("new", "y", "stuff"))
})

test_that("doesn't modify by reference", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  new_df <- df %>%
    select.(x, y)

  expect_named(df, c("x", "y", "z"))
  expect_named(new_df, c("x", "y"))
})

test_that("can select all cols and can reorder", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  new_df <- df %>%
    select.(y, new_z = z, x)

  expect_named(new_df, c("y", "new_z", "x"))
  expect_equal(new_df$y, rep(2, 3))
  expect_equal(new_df$new_z, c("a", "a", "b"))
  expect_equal(new_df$x, rep(1, 3))
})

test_that("correctly handles duplicate selection, #468", {
  df <- tidytable(x = 1, y = 2, z = 3)

  dupe1 <- df %>%
    select.(x, y, new_x = x, z)

  expect_named(dupe1, c("new_x", "y", "z"))

  dupe2 <- df %>%
    select.(x = x, y = y, new_x = x, z)

  expect_named(dupe2, c("x", "y", "new_x", "z"))
  expect_equal(dupe2$new_x, 1)
})

test_that("preserves attributes", {
  df <- data.table(x = 1, y = 2, z = 3)

  attr(df, "test_attr") <- "test"

  res <- select.(df, x, y)

  expect_equal(attr(res, "test_attr"), "test")
})
