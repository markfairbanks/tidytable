test_that("mutate_if() works for numeric columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_if(is.numeric, function(.x) .x + 1)

  expect_equal(df$x, c(2,2,2))
  expect_equal(df$y, c(3,3,3))
})

test_that("mutate_at(): .vars works with select helpers in c()", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_at(c(dt_starts_with("x")), function(.x) .x + 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(2,2,2))
})

test_that("mutate_at() works for range selection", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_at(c(x:y), function(.x) .x + 1)

  expect_equal(df$x, c(2,2,2))
  expect_equal(df$y, c(3,3,3))
})

test_that("mutate_all() works for all variables", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    dt_mutate_all(function(.x) .x + 1)

  expect_equal(df$x, c(2,2,2))
  expect_equal(df$y, c(3,3,3))
})

test_that("mutate_all() works for all variables - bare function", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_all(as.character)

  expect_equal(df$x, c("1","1","1"))
  expect_equal(df$y, c("2","2","2"))
})
