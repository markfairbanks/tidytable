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

test_that("can use names_prefix", {
  df <- data.table(x_x = 1:2, x_y = 3:4)
  pivot_df <- pivot_longer.(df, names_prefix = "x_") %>%
    arrange.(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can pivot to multiple measure cols", {
  out <- pivot_longer.(
    anscombe,
    everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )

  expect_named(out, c("set", "x", "y"))
})

test_that(".value can be at any position in `names_to`", {
  samp1 <- tidytable(
    i = 1:4,
    y_t1 = rnorm(4),
    y_t2 = rnorm(4),
    z_t1 = rep(3, 4),
    z_t2 = rep(-2, 4),
  )

  value_first <- samp1 %>%
    pivot_longer.(-i, names_to = c(".value", "time"), names_sep = "_")

  samp2 <- samp1 %>%
    rename.(t1_y = y_t1,
            t2_y = y_t2,
            t1_z = z_t1,
            t2_z = z_t2)

  value_second <- samp2 %>%
    pivot_longer.(-i, names_to = c("time", ".value"), names_sep = "_")

  expect_identical(value_first, value_second)
})

test_that("can handle missing combinations", {
  dt <- tidytable(
    id = c("A", "B"),
    x_1 = c(1, 3),
    x_2 = c(2, 4),
    y_2 = c("a", "b")
  )
  out <- pivot_longer.(dt, -id, names_to = c(".value", "n"), names_sep = "_")

  expect_named(out, c("id", "n", "x", "y"))
  expect_equal(out$x, c(1, 3, 2, 4))
  expect_equal(out$y, c("a", "b", NA, NA))
})

test_that("works with names_to = '.value'", {
  dt <- tidytable(id = 1:3, x1 = 4:6, x2 = 5:7, y1 = 7:9, y2 = 10:12)
  out <- dt %>%
    pivot_longer.(
      !id,
      names_to = ".value",
      names_pattern = "(.)."
    )

  expect_named(out, c("id", "x", "y"))
})


