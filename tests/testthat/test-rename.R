test_that("rename() works for one column", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_rename(new_x = x)

  expect_named(df, c("new_x", "y", "z"))
})

test_that("rename() works for multiple columns", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_rename(new_x = x,
              new_y = y)

  expect_named(df, c("new_x", "new_y", "z"))
})

test_that("rename_if() works with predicate", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_rename_if(is.character, function(.x) paste0(.x, "_character"))

  expect_named(df, c("x","y","z_character"))
})

test_that("rename_at(): .vars works with select helpers in c()", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_rename_at(c(dt_starts_with("x")), function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_start_append", "end_x", "z"))
})

test_that("rename_at() works for range selection", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    dt_rename_at(c(x:y), function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append", "z"))
})

test_that("rename_all() works for all variables", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2))
  df <- df %>%
    dt_rename_all(function(.x) paste0(.x, "_append"))

  expect_named(df, c("x_append", "y_append"))
})
