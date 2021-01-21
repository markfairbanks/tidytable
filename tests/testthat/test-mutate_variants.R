setup(options(lifecycle_verbosity = "quiet"))

test_that("mutate_across.(): .cols works with is.numeric", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(where(is.numeric), function(.x) .x + 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("mutate_across.(): modify-by-reference doesn't occur", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df %>%
    mutate_across.(where(is.numeric), ~ .x + 1)

  df %>%
    mutate_across.(where(is.numeric), ~ 1, .by = z)

  expect_equal(df$x_start, c(1,1,1))
  expect_equal(df$end_x, c(2,2,2))
})

test_that("mutate_across(): .cols works with is.numeric with data.frame", {
  df <- data.frame(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(where(is.numeric), function(.x) .x + 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("mutate_across(): .cols works with is.character", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(where(is.character), function(.x) paste0(.x, "_append"))

  expect_equal(df$x_start, c(1,1,1))
  expect_equal(df$z, c("a_append", "a_append", "b_append"))
})

test_that("mutate_across() works with newly named columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), list(new = function(.x) .x + 1))

  expect_named(df, c("x","y","z","x_new","y_new"))
  expect_equal(df$x_new, c(2,2,2))
  expect_equal(df$y_new, c(3,3,3))
})

test_that("mutate_across() works with newly named columns using .names with single .fn", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), ~ .x + 1, .names = "new_{.col}")

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("mutate_across() works with newly named columns using .names with single .fn using col", {
  # This test will need to be removed when {col} is deprecated
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), ~ .x + 1, .names = "new_{col}")

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("mutate_across() works with newly named columns using .names", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), list(new = function(.x) .x + 1), .names = "{.fn}_{.col}")

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("mutate_across() works with newly named columns using .names with using fn and col", {
  # This test will need to be removed when {col} and {fn} is deprecated
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), list(new = function(.x) .x + 1), .names = "{fn}_{col}")

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("mutate_across() works with newly named columns using .names w/ autonaming", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate_across.(c(x:y), list(new = ~ .x + 1, ~ .x + 2), .names = "{.col}_{.fn}_stuff")

  expect_named(df, c("x","y","z","x_new_stuff","y_new_stuff", "x_2_stuff", "y_2_stuff"))
  expect_equal(df$x_new_stuff, c(2,2,2))
  expect_equal(df$y_new_stuff, c(3,3,3))
  expect_equal(df$x_2_stuff, c(3,3,3))
  expect_equal(df$y_2_stuff, c(4,4,4))
})

# twiddle testing ----------------------------
test_that("mutate_across.() works with twiddle", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    mutate_across.(c(x:y), function(.x) .x + 1)

  twiddle_df <- df %>%
    mutate_across.(c(x:y), ~ .x + 1)

  expect_equal(anon_df, twiddle_df)
})

test_that("mutate_across() works with .by, doesn't modify-by-reference", {
  test_df <- data.table::data.table(
    x = c(1,2,3),
    y = c(4,5,6),
    z = c("a","a","b"))

  results_df <- test_df %>%
    mutate_across.(c(x, y), ~ mean(.x), .by = z)

  expect_named(results_df, c("x", "y", "z"))
  expect_equal(results_df$x, c(1.5, 1.5, 3))
  expect_equal(results_df$y, c(4.5, 4.5, 6))
  expect_equal(test_df$x, c(1,2,3))
})

test_that("mutate_across.() works with .by enhanced selection", {
  test_df <- data.table::data.table(
    x = c(1,2,3),
    y = c(4,5,6),
    z = c("a","a","b"))

  results_df <- test_df %>%
    mutate_across.(c(x, y), ~ mean(.x), .by = where(is.character))

  expect_named(results_df, c("x", "y", "z"))
  expect_equal(results_df$x, c(1.5, 1.5, 3))
  expect_equal(results_df$y, c(4.5, 4.5, 6))
})

test_that("_across.() can refer to variables in the data.table", {
  test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_across.(c(x, y), ~ .x + y)

  expect_equal(results_df$x, c(3,3,3))
  expect_equal(results_df$y, c(4,4,4))
})

test_that("_across.() can refer to variables in the data.table w/ list of .fns", {
  test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_across.(x, list(~ .x + 1, new = ~ .x + y))

  expect_equal(results_df$x_1, c(2,2,2))
  expect_equal(results_df$x_new, c(3,3,3))
})

test_that("_across.() can use bare functions", {
  test_df <- tidytable(x = 1:3, y = 2:4, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate_across.(c(x, y), between, 1, 3)

  expect_equal(results_df$x, c(TRUE, TRUE, TRUE))
  expect_equal(results_df$y, c(TRUE, TRUE, FALSE))
})
