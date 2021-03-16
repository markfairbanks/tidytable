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

test_that("rename.() works for spaced column names", {
  df <- data.table(`test spaced column` = 1:3, y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename.(new_name = `test spaced column`)

  expect_named(df, c("new_name", "y", "z"))
})

test_that("rename.() works for multiple columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    rename.(new_x = x,
            new_y = y)

  expect_named(df, c("new_x", "new_y", "z"))
})

test_that("rename_with() works for all variables", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    rename_with.(~ paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})

test_that("rename_with() doesn't modify by reference", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df %>%
    rename_with.(~ paste0(.x, "_append"))

  expect_named(df, c("x", "y"))
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
test_that("rename_with.() works with twiddle", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  anon_df <- df %>%
    rename_with.(function(.x) paste0(.x, "_append"), c(starts_with("x")))
  twiddle_df <- df %>%
    rename_with.(~ paste0(.x, "_append"), c(starts_with("x")))

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
