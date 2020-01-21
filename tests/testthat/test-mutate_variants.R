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

test_that("mutate_at(): .vars works with is.numeric", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_at(is.numeric, function(.x) .x + 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("mutate_at(): .vars works with is.character", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_at(is.character, function(.x) paste0(.x, "_append"))

  expect_equal(df$x_start, c(1,1,1))
  expect_equal(df$z, c("a_append", "a_append", "b_append"))
})

test_that("mutate_at() works with newly named columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_at(c(x:y), list(new = function(.x) .x + 1))

  expect_named(df, c("x","y","z","x_new","y_new"))
  expect_equal(df$x_new, c(2,2,2))
  expect_equal(df$y_new, c(3,3,3))
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

test_that("mutate_across(): .cols works with is.numeric", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_across(is.numeric, function(.x) .x + 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("mutate_across(): .cols works with is.character", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_across(is.character, function(.x) paste0(.x, "_append"))

  expect_equal(df$x_start, c(1,1,1))
  expect_equal(df$z, c("a_append", "a_append", "b_append"))
})

test_that("mutate_across() works with newly named columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_mutate_across(c(x:y), list(new = function(.x) .x + 1))

  expect_named(df, c("x","y","z","x_new","y_new"))
  expect_equal(df$x_new, c(2,2,2))
  expect_equal(df$y_new, c(3,3,3))
})

# twiddle testing ----------------------------
test_that("mutate_if() works with twiddle", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    dt_mutate_if(is.numeric, function(.x) .x + 1)

  twiddle_df <- df %>%
    dt_mutate_if(is.numeric, ~ .x + 1)

  expect_equal(anon_df, twiddle_df)
})

test_that("mutate_at() works with twiddle", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    dt_mutate_at(c(x:y), function(.x) .x + 1)

  twiddle_df <- df %>%
    dt_mutate_at(c(x:y), ~ .x + 1)

  expect_equal(anon_df, twiddle_df)
})

test_that("mutate_across() works with twiddle", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    dt_mutate_across(c(x:y), function(.x) .x + 1)

  twiddle_df <- df %>%
    dt_mutate_across(c(x:y), ~ .x + 1)

  expect_equal(anon_df, twiddle_df)
})
