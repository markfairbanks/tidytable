# tests from tidyr regarding pivot_longer

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

test_that("can coerce names with names_transform", {
  df <- data.table("1"=10, "2"=20)
  pivot_df <- df %>%
    pivot_longer.(1:2, names_to = "int", names_transform = list(int = as.integer))

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, c(1, 2))
  expect_equal(pivot_df$value, c(10, 20))
})

test_that("can coerce names with names_ptype", {
  df <- data.table("1"=10, "2"=20)
  pivot_df <- df %>%
    pivot_longer.(1:2, names_to = "int", names_ptype = list(int = factor()))

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, as.factor(c(1, 2)))
  expect_equal(pivot_df$value, c(10, 20))
})

test_that("can coerce values with values_transform", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- df %>%
    pivot_longer.(cols = c(x,y), values_transform = list(value = as.character)) %>%
    arrange.(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, as.character(c(1,2,3,4)))
})

test_that("can coerce values with values_ptypes", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- df %>%
    pivot_longer.(cols = c(x,y), values_ptype = list(value = int())) %>%
    arrange.(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1L,2L,3L,4L))
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
    mutate_across.(everything(), as.double)
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
  expect_error(pivot_longer.(df, c(-x,-y,-z)))
})

test_that("stops if given vector", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_error(pivot_longer.(df$x, c(x,-y)))
})

test_that("works with select helpers", {
  df <- data.table(x = 1:2, y = 2, z = 1:2) %>%
    mutate_across.(everything(), as.double)
  pivot_df <- pivot_longer.(df, cols = c(starts_with("y"), contains("z")))[order(name, value)]

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, c(1,2,1,2))
  expect_equal(pivot_df$name, c("y","y","z","z"))
  expect_equal(pivot_df$value, c(2,2,1,2))
})

test_that("names_pattern works", {
  test_df <- data.table(a1_1 = 1, b2_2 = 2)

  pivot_df <- test_df %>%
    pivot_longer.(
      names_to = c("a", "b"),
      names_pattern = "([[:alnum:]]+)_([[:alnum:]]+)"
    )

  expect_named(pivot_df, c("a", "b", "value"))
  expect_equal(pivot_df$a, c("a1", "b2"))
  expect_equal(pivot_df$b, c("1", "2"))
  expect_equal(pivot_df$value, c(1, 2))
})

test_that("can pivot all cols (specified) to long with quosure function", {
  df <- data.table(x = 1:2, y = 3:4)

  pivot_longer_fn <- function(.df, col1, col2) {
    pivot_longer.(.df, cols = c({{ col1 }}, {{ col2 }}))
  }

  pivot_df <- pivot_longer_fn(df, x, y)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})
