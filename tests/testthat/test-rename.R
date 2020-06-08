setup(options(lifecycle_verbosity = "quiet"))

test_that("dt_() works for one column", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  result_df <- df %>%
    dt_rename(new_x = x)

  expect_deprecated(dt_rename(df, new_x = x))
  expect_named(result_df, c("new_x", "y", "z"))
})

test_that("rename.() works for one column", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename.(new_x = x)

  expect_named(df, c("new_x", "y", "z"))
})

test_that("rename.() doesn't modify-by-reference", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df %>%
    rename.(new_x = x)

  expect_named(df, c("x", "y", "z"))
})

test_that("rename.() works for one column w/ data.frame", {
  df <- data.frame(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename.(new_x = x)

  expect_named(df, c("new_x", "y", "z"))
})

test_that("rename.() works for multiple columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename.(new_x = x,
            new_y = y)

  expect_named(df, c("new_x", "new_y", "z"))
})

test_that("rename_if() works with predicate", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename_if.(is.character, function(.x) paste0(.x, "_character"))

  expect_named(df, c("x","y","z_character"))
})

test_that("rename_at(): .vars works with select helpers in c()", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename_at.(c(dt_starts_with("x")), function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_start_append", "end_x", "z"))
})

test_that("rename_at() works for range selection", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename_at.(c(x:y), function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append", "z"))
})

test_that("rename_all() works for all variables", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    rename_all.(function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})

test_that("rename_all() works for all variables w/ data.frame", {
  df <- data.frame(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    rename_all.(function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})

test_that("rename_with() works for all variables", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    rename_with.(~ paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})

test_that("rename_with() works for all variables w/ data.frame", {
  df <- data.frame(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    rename_with.(~ paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})

test_that("rename_if() works with predicate", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename_with.(~ paste0(.x, "_character"), where(is.character))

  expect_named(df, c("x","y","z_character"))
})

# twiddle testing -----------------------------------
test_that("rename_across() works with twiddle", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    as_tidytable() %>%
    dt_rename_across(c(dt_starts_with("x")), ~ paste0(.x, "_append"))
  twiddle_df <- df %>%
    as_tidytable() %>%
    dt_rename_across(c(dt_starts_with("x")), ~ paste0(.x, "_append"))

  expect_equal(anon_df, twiddle_df)
})

test_that("rename_all() works with twiddle", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  anon_df <- df %>%
    as_tidytable() %>%
    dt_rename_all(function(.x) paste0(.x, "_append"))

  twiddle_df <- df %>%
    as_tidytable() %>%
    dt_rename_all(~ paste0(.x, "_append"))

  expect_named(twiddle_df, c("x_append", "y_append"))
  expect_equal(anon_df, twiddle_df)
})

test_that("can make a custom function with quosures", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))

  rename_fn <- function(data, new_name, old_name) {
    data %>%
      rename.({{new_name}} := {{old_name}})
  }

  df <- df %>%
    rename_fn(new_x, x)

  expect_named(df, c("new_x", "y", "z"))
})
