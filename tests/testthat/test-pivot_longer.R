# tests from tidyr regarding pivot_longer

test_that("dt_ can pivot all cols (unspecified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- dt_pivot_longer(df)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can pivot all cols (unspecified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can pivot all cols (unspecified) to long with data.frame", {
  df <- data.frame(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})


test_that("can pivot all cols (specified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df, cols = c(x,y))[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can select a single column", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df, cols = x)[order(name, value)]

  expect_named(pivot_df, c("y", "name", "value"))
  expect_equal(pivot_df$name, c("x","x"))
  expect_equal(pivot_df$value, c(1,2))

  pivot_df2 <- pivot_longer.(df, cols = c(x))[order(name, value)]
  expect_equal(pivot_df, pivot_df2)
})

test_that("preserves original keys", {
  df <- data.table(x = 1:2, y = 2, z = 1:2) %>%
    mutate_all.(as.double)
  pivot_df <- pivot_longer.(df, cols = c(y, z))[order(name, value)]

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, c(1,2,1,2))
})

test_that("can drop missing values", {
  df <- data.table(x = c(1, NA), y = c(NA, 2))
  pivot_df <- pivot_longer.(df, c(x,y), values_drop_na = TRUE)[order(name, value)]

  expect_equal(pivot_df$name, c("x", "y"))
  expect_equal(pivot_df$value, c(1,2))
})

test_that("... args to melt", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_named(pivot_longer.(df, c(x,y), verbose = TRUE), c("name", "value"))
})

test_that("testing removal of multiple columns", {
  df <- data.table(x = c(1, 2), y = c(2,2), z = c(1,1))
  expect_named(pivot_longer.(df, c(-x)), c("x", "name", "value"))
  expect_named(pivot_longer.(df, -x), c("x", "name", "value"))
  expect_named(pivot_longer.(df, c(-x,-y)), c("x", "y", "name", "value"))
  expect_warning(pivot_longer.(df, c(-x,-y,-z)))
})

test_that("stops if given vector", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_error(dt_pivot_longer(df$x, c(x,-y)))
})

test_that("works with select helpers", {
  df <- data.table(x = 1:2, y = 2, z = 1:2) %>%
    mutate_across.(everything.(), as.double)
  pivot_df <- dt_pivot_longer(df, cols = c(dt_starts_with("y"), dt_contains("z")))[order(name, value)]

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, c(1,2,1,2))
  expect_equal(pivot_df$name, c("y","y","z","z"))
  expect_equal(pivot_df$value, c(2,2,1,2))
})

test_that("a single helper works outside of c() call", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df, cols = everything.())[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})

test_that("can pivot all cols (specified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df, cols = is.numeric)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})
