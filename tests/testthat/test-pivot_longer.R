# tests from tidyr regarding pivot_longer

test_that("can pivot all cols (unspecified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer(df)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})

test_that("pivot_longer. works", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer.(df) %>% suppressWarnings()

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})

test_that("can pivot all cols (unspecified) to long with data.frame", {
  df <- data.frame(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer(df)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})

test_that("doesn't convert character cols to factor", {
  df <- data.table(x = c("a", "b"), y = c("a", "b"))
  pivot_df <- pivot_longer(df)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c("a", "b", "a", "b"))
})

test_that("can pivot all cols (specified) to long", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- pivot_longer(df, cols = c(x,y))

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, 1:4)
})

test_that("can coerce names with names_transform", {
  df <- data.table("1"=10, "2"=20)
  # transform in a list
  pivot_df <- df %>%
    pivot_longer(1:2, names_to = "int", names_transform = list(int = as.integer))

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, c(1, 2))
  expect_true(is.integer(pivot_df$int))
  expect_equal(pivot_df$value, c(10, 20))

  # Can pass single transform
  pivot_df <- df %>%
    pivot_longer(1:2, names_to = "int", names_transform = as.integer)

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, c(1, 2))
  expect_true(is.integer(pivot_df$int))
  expect_equal(pivot_df$value, c(10, 20))
})

test_that("can coerce names with names_ptype", {
  df <- data.table("1"=10, "2"=20)
  pivot_df <- df %>%
    pivot_longer(1:2, names_to = "int", names_ptype = list(int = factor()))

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, as.factor(c(1, 2)))
  expect_equal(pivot_df$value, c(10, 20))

  pivot_df <- df %>%
    pivot_longer(1:2, names_to = "int", names_ptype = factor())

  expect_named(pivot_df, c("int", "value"))
  expect_equal(pivot_df$int, as.factor(c(1, 2)))
  expect_equal(pivot_df$value, c(10, 20))
})

test_that("can coerce values with values_transform", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- df %>%
    pivot_longer(cols = c(x,y), values_transform = list(value = as.character)) %>%
    arrange(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, as.character(c(1,2,3,4)))

  # Passing a single function
  pivot_df <- df %>%
    pivot_longer(cols = c(x,y), values_transform = as.character) %>%
    arrange(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, as.character(c(1,2,3,4)))
})

test_that("can coerce values with values_ptypes", {
  df <- data.table(x = 1:2, y = 3:4)
  pivot_df <- df %>%
    pivot_longer(cols = c(x,y), values_ptype = list(value = int())) %>%
    arrange(name, value)

  pivot_df <- df %>%
    pivot_longer(cols = c(x,y), values_ptype = int()) %>%
    arrange(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1L,2L,3L,4L))
})

test_that("preserves original keys", {
  df <- data.table(x = 1:2, y = 2, z = 1:2) %>%
    mutate(across(everything(), as.double))
  pivot_df <- pivot_longer(df, cols = c(y, z))

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, c(1,2,1,2))
})

test_that("can drop missing values", {
  df <- data.table(x = c(1, NA), y = c(NA, 2))
  pivot_df <- pivot_longer(df, c(x,y), values_drop_na = TRUE)

  expect_equal(pivot_df$name, c("x", "y"))
  expect_equal(pivot_df$value, c(1,2))
})

test_that("drops using | condition when using .value", {
  df <- data.table(x_1 = 1, x_2 = 2,  y_1 = NA, y_2 = 2)
  pivot_df <- df %>%
    pivot_longer(names_to = c(".value", "id"), names_sep = "_", values_drop_na = TRUE)

  expect_named(pivot_df, c("id", "x", "y"))
  expect_equal(pivot_df$y, c(NA,2))

  df <- data.table(x_1 = NA, x_2 = 2,  y_1 = NA, y_2 = 2)
  pivot_df <- df %>%
    pivot_longer(names_to = c(".value", "id"), names_sep = "_", values_drop_na = TRUE)

  expect_named(pivot_df, c("id", "x", "y"))
  expect_equal(pivot_df$y, 2)
})

test_that("... args to melt", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_named(pivot_longer(df, c(x,y), verbose = TRUE), c("name", "value"))
})

test_that("testing removal of multiple columns", {
  df <- data.table(x = c(1, 2), y = c(2,2), z = c(1,1))
  expect_named(pivot_longer(df, c(-x)), c("x", "name", "value"))
  expect_named(pivot_longer(df, -x), c("x", "name", "value"))
  expect_named(pivot_longer(df, c(-x,-y)), c("x", "y", "name", "value"))
  expect_error(pivot_longer(df, c(-x,-y,-z)))
})

test_that("stops if given vector", {
  df <- data.table(x = c(1, 2), y = c(2,2))
  expect_error(pivot_longer(df$x, c(x,-y)))
})

test_that("works with select helpers", {
  df <- data.table(x = 1:2, y = 2, z = 1:2) %>%
    mutate(across(everything(), as.double))
  pivot_df <- pivot_longer(df, cols = c(starts_with("y"), contains("z")))[order(name, value)]

  expect_named(pivot_df, c("x", "name", "value"))
  expect_equal(pivot_df$x, c(1,2,1,2))
  expect_equal(pivot_df$name, c("y","y","z","z"))
  expect_equal(pivot_df$value, c(2,2,1,2))
})

test_that("names_pattern works", {
  test_df <- data.table(a1_1 = 1, b2_2 = 2)

  pivot_df <- test_df %>%
    pivot_longer(
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
    pivot_longer(.df, cols = c({{ col1 }}, {{ col2 }}))
  }

  pivot_df <- pivot_longer_fn(df, x, y)[order(name, value)]

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can use names_prefix", {
  df <- data.table(x_x = 1:2, x_y = 3:4)
  pivot_df <- pivot_longer(df, names_prefix = "x_") %>%
    arrange(name, value)

  expect_named(pivot_df, c("name", "value"))
  expect_equal(pivot_df$name, c("x","x","y","y"))
  expect_equal(pivot_df$value, c(1,2,3,4))
})

test_that("can pivot to multiple measure cols", {
  test_df <- tidytable(x_3 = 3, x_4 = 4, y_3 = 3, y_4 = 4)

  out <- test_df %>%
    pivot_longer(names_to = c(".value", "id"), names_sep = "_")

  expect_named(out, c("id", "x", "y"))
  expect_equal(out$id, c("3", "4"))
})

test_that("can drop the 'id' column by specifying NA", {
  test_df <- tidytable(x_3 = 3, x_4 = 4, y_3 = 3, y_4 = 4)

  out <- test_df %>%
    pivot_longer(names_to = c(".value", NA), names_sep = "_")

  expect_named(out, c("x", "y"))
  expect_equal(out$x, c(3, 4))
  expect_equal(out$y, c(3, 4))
})

test_that("balanced data can have different column order", {
  test_df <- tidytable(x_3 = 3, x_4 = 4, y_4 = 4, y_3 = 3)

  out <- test_df %>%
    pivot_longer(names_to = c(".value", "id"), names_sep = "_")

  expect_named(out, c("id", "x", "y"))
  expect_equal(out$id, c("3", "4"))
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
    pivot_longer(-i, names_to = c(".value", "time"), names_sep = "_")

  samp2 <- samp1 %>%
    rename(t1_y = y_t1,
            t2_y = y_t2,
            t1_z = z_t1,
            t2_z = z_t2)

  value_second <- samp2 %>%
    pivot_longer(-i, names_to = c("time", ".value"), names_sep = "_")

  expect_identical(value_first, value_second)
})

test_that("works with unbalanced data - 1", {
  dt <- tidytable(
    id = c("A", "B"),
    x_1 = c(1, 3),
    x_2 = c(2, 4),
    y_2 = c("a", "b")
  )
  out <- pivot_longer(dt, -id, names_to = c(".value", "n"), names_sep = "_")

  expect_named(out, c("id", "n", "x", "y"))
  expect_equal(out$x, c(1, 3, 2, 4))
  expect_equal(out$y, c(NA, NA, "a", "b"))
})

test_that("works with unbalanced data - 2", {
  test_df <- tidytable(x2 = 2, x3 = 3, y5 = 5, y6 = 6)
  out <- test_df %>%
    pivot_longer(names_to = c(".value", "id"), names_pattern = "(.)(.)")

  expect_named(out, c("id", "x", "y"))
  expect_equal(out$id, c("2", "3", "5", "6"))
  expect_equal(out$x, c(2, 3, NA, NA))
  expect_equal(out$y, c(NA, NA, 5, 6))
})

test_that("works with unbalanced data - 3", {
  set.seed(2334)
  test_df <- data.table(
    a_alpha = rnorm(3), a_gamma = rnorm(3),
    b_beta = rnorm(3), b_gamma = rnorm(3),
    groups = 1:3
  )

  out <- test_df %>%
    pivot_longer(-groups, names_to = c(".value", "labels"), names_sep = "_")

  expect_equal(out$groups, rep(1:3, 3))
  expect_equal(out$labels, c(rep("alpha", 3), rep("beta", 3), rep("gamma", 3)))
  expect_equal(
    round(out$a, 3),
    c(-0.118, 1.237, 0.809, NA, NA, NA, -0.766, -0.592, 0.528)
  )
  expect_equal(
    round(out$b, 3),
    c(NA, NA, NA, 1.682, -0.574, -0.057, -0.706, 0.002, 1.064)
  )
})

test_that("errors with `names_to = '.value'`", {
  dt <- tidytable(x1 = 1, x2 = 2, y1 = 1, y2 = 2)

  expect_error(
    dt %>%
      pivot_longer(
        !id,
        names_to = ".value",
        names_pattern = "(.)."
      )
  )
})

test_that("doesn't convert factor cols to character, #202", {
  fct_df <- tidytable(x = factor("a"), y = factor("b"))
  out <- pivot_longer(fct_df)

  expect_equal(out$value, as.factor(c("a", "b")))
})

test_that("doesn't convert factor cols to character, #234.
          Preserves column order, #235", {
  df <- tidytable(
    fct_1 = factor("a"),
    fct_2 = factor("b"),
    dbl_1 = 1,
    dbl_2 = 2,
    chr_1 = "a",
    chr_2 = "b"
  )

  out <- df %>%
    pivot_longer(names_to = c(".value", "id"), names_sep = "_")

  expect_named(out, c("id", "fct", "dbl", "chr"))
  expect_equal(out$fct, as.factor(c("a", "b")))
  expect_equal(out$chr, c("a", "b"))
  expect_equal(out$dbl, c(1, 2))
})




