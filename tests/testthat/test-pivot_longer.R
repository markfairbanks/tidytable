# tests from tidyr regarding pivot_longer


test_that("can pivot all cols (unspecified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df)[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, cols = c(x,y)), name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, tidyr_df$name)
  expect_equal(pivot_df$value, tidyr_df$value)
})


test_that("can pivot all cols (specified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df, cols = c(x,y))[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, cols = c(x, y)), name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, tidyr_df$name)
  expect_equal(pivot_df$value, tidyr_df$value)
})

test_that("can select a single column", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df, cols = x)[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, cols = x), name, value)

  expect_named(pivot_df, c("y", "name", "value"))
  expect_equal(pivot_df$name, tidyr_df$name)
  expect_equal(pivot_df$value, tidyr_df$value)

  pivot_df2 <- dt_pivot_longer(df, cols = c(x))[order(name, value)]
  expect_equal(pivot_df, pivot_df2)
})

test_that("preserves original keys", {
  df <- data.table(x = 1:2, y = 2, z = 1:2)
  pivot_df <- dt_pivot_longer(df, cols = c(y, z))[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, c(y, z)), name, value)

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, tidyr_df$x)
})

test_that("can drop missing values", {
  df <- data.table(x = c(1, NA), y = c(NA, 2))
  pivot_df <- dt_pivot_longer(df, c(x,y), values_drop_na = TRUE)[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, c(x,y), values_drop_na = TRUE), name, value)

  expect_equal(pivot_df$name, c("x", "y"))
  expect_equal(pivot_df$value, tidyr_df$value)
})

test_that("... args to melt", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_named(dt_pivot_longer(df, c(x,y), verbose = TRUE), c("name", "value"))
})

test_that("testing removal of multiple columns", {
  df <- data.table(x = c(1, 2), y = c(2,2), z = c(1,1))
  expect_named(dt_pivot_longer(df, c(-x)), c("x", "name", "value"))
  expect_named(dt_pivot_longer(df, -x), c("x", "name", "value"))
  expect_named(dt_pivot_longer(df, c(-x,-y)), c("x", "y", "name", "value"))
  expect_warning(dt_pivot_longer(df, c(-x,-y,-z)))
})

test_that("stops if given vector", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_error(dt_pivot_longer(df$x, c(x,-y)))
})

test_that("works with select helpers", {
  df <- data.table(x = 1:2, y = 2, z = 1:2)
  pivot_df <- dt_pivot_longer(df, cols = c(dt_starts_with("y"), dt_contains("z")))[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, c(dplyr::starts_with("y"), dplyr::contains("z"))), name, value)

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, tidyr_df$x)
})

test_that("a single helper works outside of c() call", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df, cols = dt_everything())[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, cols = dplyr::everything()), name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, tidyr_df$name)
  expect_equal(pivot_df$value, tidyr_df$value)
})

test_that("can pivot all cols (specified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df, cols = is.numeric)[order(name, value)]
  tidyr_df <- dplyr::arrange(tidyr::pivot_longer(df, cols = c(x, y)), name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, tidyr_df$name)
  expect_equal(pivot_df$value, tidyr_df$value)
})
