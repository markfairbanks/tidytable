# tests from tidyr regarding pivot_wider

test_that("can pivot all cols to wide", {
  df <- data.table(label = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider(df, names_from = label, values_from = val)

  expect_named(pivot_df, c("x", "y", "z"))
  expect_equal(nrow(pivot_df), 1)
  expect_false(data.table::haskey(pivot_df))
})

test_that("`names_sort = FALSE` works", {
  df <- tidytable(id = 1, names = c("b", "a", "c"), values = c(2, 1, 3))
  pivot_df <- pivot_wider(df, names_from = names, values_from = values)

  expect_named(pivot_df, c("id", "b", "a", "c"))
  expect_equal(unlist(pivot_df, use.names = FALSE), c(1, 2, 1, 3))
})

test_that("pivot_wider. works", {
  df <- data.table(label = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider.(df, names_from = label, values_from = val)

  expect_named(pivot_df, c("x", "y", "z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("non-pivoted cols are preserved", {
  df <- data.table(a = 1, label = c("x", "y"), val = 1:2)
  pivot_df <- pivot_wider(df, names_from = label, values_from = val)

  expect_named(pivot_df, c("a", "x", "y"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("implicit missings turn into explicit missings", {
  df <- data.table(a = 1:2, label = c("x", "y"), val = 1:2)
  pivot_df <- pivot_wider(df, names_from = label, values_from = val)

  expect_equal(pivot_df$a, c(1, 2))
  expect_equal(pivot_df$x, c(1, NA))
  expect_equal(pivot_df$y, c(NA, 2))
})

test_that("can override default keys", {
  df <- data.table(row = 1:3,
                   name = c("Sam", "Sam", "Bob"),
                   var = c("age", "height", "age"),
                   value = c(10, 1.5, 20))

  pv <- pivot_wider(df, id_cols = name, names_from = var, values_from = value)
  expect_equal(nrow(pv), 2)
})

# multiple values ----------------------------------------------------------

test_that("can pivot from multiple measure cols", {
  df <- data.table(row = 1, var = c("x", "y"), a = 1:2, b = 3:4)
  pv <- pivot_wider(df, names_from = var, values_from = c(a, b))

  expect_named(pv, c("row", "a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

test_that("can pivot from multiple measure cols using all keys", {
  df <- data.table(var = c("x", "y"), a = 1:2, b = 3:4)
  pv <- pivot_wider(df, names_from = var, values_from = c(a, b))

  expect_named(pv, c("a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

# select helpers ----------------------------------------------------------

test_that("can pivot from multiple measure cols using helpers", {
  df <- data.table(row = 1, var = c("x", "y"), a = 1:2, b = 3:4)
  pv <- pivot_wider(
    df,
    names_from = var,
    values_from = c(starts_with("a"), ends_with("b"))
  )

  expect_named(pv, c("row", "a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

# names args ----------------------------------------------------------
test_that("can add a prefix", {
  df <- data.table(label = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider(
    df, names_from = label, values_from = val, names_prefix = "test_"
  )

  expect_named(pivot_df, c("test_x", "test_y", "test_z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("can add a prefix - multiple names_from", {
  df <- data.table(label1 = c("x", "y", "z"), label2 = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider(
    df, names_from = c(label1, label2),
    values_from = val,
    names_prefix = "test_"
  )

  expect_named(pivot_df, c("test_x_x", "test_y_y", "test_z_z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("can use names_glue", {
  df <- data.table(label = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider(
    df, names_from = label, values_from = val, names_glue = "test_{label}"
  )

  expect_named(pivot_df, c("test_x", "test_y", "test_z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("can use names_glue - multiple names_from", {
  df <- data.table(label1 = c("x", "y", "z"), label2 = c("x", "y", "z"), val = 1:3)
  pivot_df <- pivot_wider(
    df, names_from = c(label1, label2), values_from = val,
    names_glue = "test_{label1}_{label2}"
  )

  expect_named(pivot_df, c("test_x_x", "test_y_y", "test_z_z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("names_glue works with .value", {
  df <- data.table(
    x = c("X", "Y"),
    y = 1:2,
    a = 1:2,
    b = 1:2
  )

  out <- pivot_wider(df, names_from = x:y, values_from = a:b, names_glue = "{x}{y}_{.value}")
  expect_named(out, c("X1_a", "Y2_a", "X1_b", "Y2_b"))
})

test_that("can sort names", {
  df <- data.table(label = c("z", "y", "x"), val = 1:3)
  pivot_df <- pivot_wider(
    df, names_from = label, values_from = val,
    names_glue = "test_{label}", names_sort = TRUE
  )

  expect_named(pivot_df, c("test_x", "test_y", "test_z"))
  expect_equal(nrow(pivot_df), 1)
})

# using values_fn ----------------------------------------------------------
df <- data.table(a = c(1, 1, 2), stuff = c("x", "x", "x"), val = c(1, 10, 100))

test_that("works with is.numeric helper", {
  df <- data.table(a = c(1, 1, 2), stuff = c("x", "x", "x"), val = c(1, 10, 100))

  pivot_df <- pivot_wider(df, names_from = stuff, values_from = val, values_fn = sum)

  expect_equal(pivot_df$a, c(1, 2))
  expect_equal(pivot_df$x, c(11, 100))
})

test_that("can pivot all cols to wide with quosure function", {
  df <- data.table(label = c("x", "y", "z"), val = 1:3)

  pivot_wider_fn <- function(.df, names, values) {
    pivot_wider(df, names_from = {{ names }}, values_from = {{ values }})
  }

  pivot_df <- pivot_wider_fn(df, names = label, values = val)

  expect_named(pivot_df, c("x", "y", "z"))
  expect_equal(nrow(pivot_df), 1)
})

test_that("can fill in missing cells", {
  df <- data.table(g = c(1, 2), var = c("x", "y"), val = c(1, 2))

  widen <- function(...) {
    df %>% pivot_wider(names_from = var, values_from = val, ...)
  }

  expect_equal(widen()$x, c(1, NA))
  expect_equal(widen(values_fill = 0)$x, c(1, 0))
  expect_equal(widen(values_fill = list(val = 0))$x, c(1, 0))
})

test_that("values_fill only affects missing cells", {
  df <- tidytable(g = c(1, 2), names = c("x", "y"), value = c(1, NA))
  out <- pivot_wider(df, names_from = names, values_from = value, values_fill = 0)
  expect_equal(out$y, c(0, NA))
})

test_that("can pivot data frames with spaced names, #569", {
  df <- tidytable("a a" = 1,
                  "names" = c("a", "b"),
                  "vals" = 1:2)
  out <- pivot_wider(df, names_from = names, values_from = vals)
  expect_named(out, c("a a", "a", "b"))
})

# names_glue column order ----------------------------------------------------------
test_that("correctly labels columns when `names_glue` is used, #579", {
  # length(values_from) == 1
  df1 <- tidytable(
    lettr = c("b", "a", "c"),
    v1 = c("b", "a", "c")
  )

  result1 <- pivot_wider.(
    df1,
    names_from = lettr,
    values_from = v1,
    names_glue = "{.value}_{lettr}"
  )

  expect_named(result1, c("v1_b", "v1_a", "v1_c"))
  expect_equal(unname(unlist(result1)), c("b", "a", "c"))

  # length(values_from) > 1
  df2 <- tidytable(
    lettr = c("b", "a", "c"),
    v1 = c("b", "a", "c"),
    v2 = c("b", "a", "c")
  )

  result2 <- pivot_wider.(
    df2,
    names_from = lettr,
    values_from = c(v1, v2),
    names_glue = "{.value}_{lettr}"
  )

  expect_named(result2, c("v1_b", "v1_a", "v1_c", "v2_b", "v2_a", "v2_c"))
  expect_equal(unname(unlist(result2)), c("b", "a", "c", "b", "a", "c"))
})
