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

test_that("works with dates", {
  df <- tidytable(employee = c("Bob", "Cindy", "Murph"),
                  employee_id = 1:3,
                  start_date = as.Date(paste0("2020-01-0", c(3, 1, 2))))

  res <- pivot_wider(df, c(employee, start_date), employee_id)

  expect_named(res, c("Bob_2020-01-03", "Cindy_2020-01-01", "Murph_2020-01-02"))
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

  result1 <- pivot_wider(
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

  result2 <- pivot_wider(
    df2,
    names_from = lettr,
    values_from = c(v1, v2),
    names_glue = "{.value}_{lettr}"
  )

  expect_named(result2, c("v1_b", "v1_a", "v1_c", "v2_b", "v2_a", "v2_c"))
  expect_equal(unname(unlist(result2)), c("b", "a", "c", "b", "a", "c"))
})

# unused -------------------------------------------------------------------

test_that("only uses used columns when `unused_fn = NULL`, #698", {
  df <- data.frame(
    a   = LETTERS[1:2],
    b   = LETTERS[3:4],
    val = 1:2
  )

  res <- df %>%
    pivot_wider(
      id_cols = character(0),
      names_from = a,
      values_from = val
    )

  expect_named(res, c("A", "B"))
  expect_equal(res$A, 1)
  expect_equal(res$B, 2)
})

test_that("`unused_fn` can summarize unused columns (#990)", {
  df <- tidytable(
    id = c(1, 1, 2, 2),
    unused1 = c(1, 2, 4, 3),
    unused2 = c(1, 2, 4, 3),
    name = c("a", "b", "a", "b"),
    value = c(1, 2, 3, 4)
  )

  # # By name
  # res <- pivot_wider(df, id_cols = id, unused_fn = list(unused1 = max))
  # expect_named(res, c("id", "a", "b", "unused1"))
  # expect_identical(res$unused1, c(2, 4))

  # Globally
  res <- pivot_wider(df, id_cols = id, unused_fn = list)
  expect_named(res, c("id", "a", "b", "unused1", "unused2"))
  expect_identical(res$unused1, list(c(1, 2), c(4, 3)))
  expect_identical(res$unused2, list(c(1, 2), c(4, 3)))

  # https://stackoverflow.com/a/73554147
  df <- data.frame(A = c(1, 1, 1, 2 , 2, 2),
                   B = c(3, 3, 3, 6, 6, 6),
                   C = c(2, 3, 9, 12, 2, 6),
                   D = c("a1", "a2", "a3", "a1", "a2", "a3"))

  res <- df %>%
    pivot_wider(id_cols = A, names_from = D, values_from = C, unused_fn = mean)
  expect_named(res, c("A", "a1", "a2", "a3", "B"))
  expect_equal(res$B, c(3, 6))

  # Works with anonymous functions
  res <- df %>%
    pivot_wider(id_cols = A, names_from = D, values_from = C, unused_fn = ~ mean(.x))
  expect_named(res, c("A", "a1", "a2", "a3", "B"))
  expect_equal(res$B, c(3, 6))
})

test_that("`unused_fn` works with anonymous functions", {
  df <- tidytable(
    id = c(1, 1, 2, 2),
    unused = c(1, NA, 4, 3),
    name = c("a", "b", "a", "b"),
    value = c(1, 2, 3, 4)
  )

  res <- pivot_wider(df, id_cols = id, unused_fn = ~ mean(.x, na.rm = TRUE))
  expect_identical(res$unused, c(1, 3.5))
})

# test_that("`unused_fn` must result in single summary values", {
#   df <- tidytable(
#     id = c(1, 1, 2, 2),
#     unused = c(1, 2, 4, 3),
#     name = c("a", "b", "a", "b"),
#     value = c(1, 2, 3, 4)
#   )
#
#   expect_snapshot(
#     (expect_error(pivot_wider(df, id_cols = id, unused_fn = identity)))
#   )
# })

# test_that("`unused_fn` works with expanded key from `id_expand`", {
#   df <- tidytable(
#     id = factor(c(1, 1, 2, 2), levels = 1:3),
#     unused = c(1, 2, 4, 3),
#     name = c("a", "b", "a", "b"),
#     value = c(1, 2, 3, 4)
#   )
#
#   res <- pivot_wider(df, id_cols = id, id_expand = TRUE, unused_fn = max)
#   expect_identical(res$id, factor(1:3))
#   expect_identical(res$unused, c(2, 4, NA))
#
#   res <- pivot_wider(df, id_cols = id, id_expand = TRUE, unused_fn = ~ sum(is.na(.x)))
#   expect_identical(res$unused, c(0L, 0L, 1L))
# })

# test_that("can't fill implicit missings in unused column with `values_fill`", {
#   # (in theory this would need `unused_fill`, but it would only be used when
#   # `id_expand = TRUE`, which doesn't feel that useful)
#
#   df <- tidytable(
#     id = factor(c(1, 1, 2, 2), levels = 1:3),
#     unused = c(1, 2, 4, 3),
#     name = c("a", "b", "a", "b"),
#     value = c(1, 2, 3, 4)
#   )
#
#   res <- pivot_wider(
#     data = df,
#     id_cols = id,
#     id_expand = TRUE,
#     unused_fn = list,
#     values_fill = 0
#   )
#
#   expect_identical(res$a, c(1, 3, 0))
#   expect_identical(res$b, c(2, 4, 0))
#   expect_identical(res$unused, list(c(1, 2), c(4, 3), NA_real_))
#
#   res <- pivot_wider(
#     data = df,
#     id_cols = id,
#     id_expand = TRUE,
#     unused_fn = list,
#     values_fill = list(unused = 0)
#   )
#
#   expect_identical(res$unused, list(c(1, 2), c(4, 3), NA_real_))
# })
