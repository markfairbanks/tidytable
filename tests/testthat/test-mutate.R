test_that("can remove variables with NULL", {
  df <- data.table(x = rep(1, 3), y = rep(2, 3))
  tidytable_df <- mutate(df, y = NULL)

  # Check that .by with NULL works
  # Only deletes if the the NULL is in the last position
  tidytable2_df <- df %>%
    mutate(x = NULL,
            x = rep(1, 3),
            x_plus_y = x + y,
            y = NULL,
            .by = x)

  df_check <- tidytable(x = rep(1, 3), x_plus_y = rep(3, 3))

  expect_named(tidytable_df, c("x"))
  expect_equal(tidytable2_df, df_check)

  # if it doesn't exist
  expect_warning(df %>% mutate(z = NULL))
})

test_that("can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(double_x = x * 2,
           double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
  expect_equal(df$y * 2, df$double_y)
})

test_that("mutate. works", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
  expect_equal(df$y * 2, df$double_y)
})

test_that("modify-by-reference doesn't occur", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y"))
})

test_that("modify-by-reference doesn't occur with single val", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate(x = 1)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("modify-by-reference doesn't occur with single val variable", {
  df <- data.table(x = 1:3, y = 1:3)

  new_val <- 1

  df %>%
    mutate(x = !!new_val)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("column order is correct", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(double_x = x * 2,
            x = 1)

  expect_named(df, c("x", "y", "double_x"))
})

test_that("can take data.frame input", {
  df <- data.frame(x = 1:3, y = 1:3)
  out <- df %>%
    mutate(double_x = x * 2,
            y = 1)

  expect_named(out, c("x", "y", "double_x"))
  expect_equal(out$double_x, c(2, 4, 6))
  expect_equal(out$y, c(1, 1, 1))
  # Doesn't modify-by-reference
  expect_equal(df$y, 1:3)
})

test_that("can use .by", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate(z = mean(x), .by = y)

  datatable_df <- copy(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)
})

test_that("works on grouped_tt", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  res <- df %>%
    group_by(y) %>%
    mutate(z = mean(x))

  check <- df %>%
    mutate(z = mean(x), .by = y)

  expect_equal(ungroup(res), check)
  expect_equal(group_vars(res), "y")
  expect_true(is_grouped_df(res))
})

test_that("can mutate in order with .by", {
  df <- tidytable(x = rep(1, 3), z = c("a", "a", "b"))

  tidytable_df <- df %>%
    mutate(x = x + 1, y = x + 1, .by = z)

  expect_equal(tidytable_df$y, rep(3, 3))
})

test_that("modify-by-reference doesn't occur with single val with .by", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate(x = 1, .by = y)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("can use .N", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(z = .N)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(3,3,3))
})

test_that("can use .N with .by", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate(z = .N, .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(2,2,1))
})

test_that("can use .N in existing col", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(x = .N)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(3,3,3))
})

test_that("can use n()", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate(z = n())

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(3,3,3))
})

test_that("can use n() with by", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate(z = n(), .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(2,2,1))
})

test_that("can use .GRP", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate(z = .GRP, .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(1,1,2))
})

test_that("can use .y in map2() in nested data.tables", {
  test_df <- data.table(
    id = seq(1, 3),
    val_1 = seq(1, 3, 1),
    val_2 = seq(4, 6, 1)
  )

  result_df1 <- test_df %>%
    nest_by(id) %>%
    mutate(example_1 = map2(data, id,
                              ~ mutate(.x, id1 = .y))) %>%
    unnest(example_1)

  expect_named(result_df1, c("id","val_1", "val_2", "id1"))
  expect_equal(result_df1$id1, c(1,2,3))

  result_df2 <- test_df %>%
    nest_by(id) %>%
    mutate(example_1 = map2(data, id,
                              ~ .x %>% mutate(id1 = .y))) %>%
    unnest(example_1)

  expect_named(result_df2, c("id","val_1", "val_2", "id1"))
  expect_equal(result_df2$id1, c(1,2,3))
})

test_that("can make custom functions with quosures", {
  df <- data.table(x = c(1,2,3), y = c(1,1,1), z = c("a","a","b"))

  add_one <- function(.data, add_col, new_name, val, by) {
    val <- val
    .data %>%
      mutate({{ new_name }} := {{ add_col }} + val, .by = {{ by }})
  }

  result_df <- df %>%
    add_one(x, stuff, 1, z)

  expect_named(result_df, c("x", "y", "z", "stuff"))
  expect_equal(result_df$stuff, c(2,3,4))
})

# .before, .after, .keep ------------------------------------------------------

test_that(".keep = 'unused' keeps variables explicitly mentioned", {
  df <- tidytable(x = 1, y = 2)
  out <- mutate(df, x1 = x + 1, y = y, .keep = "unused")
  expect_named(out, c("y", "x1"))
})

test_that(".keep = 'used' not affected by across()", {
  df <- tidytable(x = 1, y = 2, z = 3, a = "a", b = "b", c = "c")

  # This must evaluate every column in order to figure out if should
  # be included in the set or not, but that shouldn't be counted for
  # the purposes of "used" variables
  out <- mutate(df, across(where(is.numeric), identity), .keep = "unused")
  expect_named(out, names(df))
})

test_that(".keep = 'used' keeps variables used in expressions", {
  df <- tidytable(a = 1, b = 2, c = 3, x = 1, y = 2)
  out <- mutate(df, xy = x + y, .keep = "used")
  expect_named(out, c("x", "y", "xy"))
})

test_that(".keep = 'none' only keeps grouping variables", {
  df <- tidytable(x = 1, y = 2)

  expect_named(mutate(df, z = 1, .keep = "none"), "z")
  expect_named(mutate(df, z = 1, .by = x, .keep = "none"), c("x", "z"))
})

test_that(".keep= always retains grouping variables (#5582)", {
  df <- tidytable(x = 1, y = 2, z = 3)
  expect_equal(
    mutate(df, a = x + 1, .keep = "none", .by = z),
    tidytable(z = 3, a = 2)
  )
  expect_equal(
    mutate(df, a = x + 1, .keep = "all", .by = z),
    tidytable(x = 1, y = 2, z = 3, a = 2)
  )
  expect_equal(
    mutate(df, a = x + 1, .keep = "used", .by = z),
    tidytable(x = 1, z = 3, a = 2)
  )
  expect_equal(
    mutate(df, a = x + 1, .keep = "unused"),
    tidytable(y = 2, z = 3, a = 2)
  )
})

test_that("can use .before and .after to control column position", {
  df <- tidytable(x = 1, y = 2)
  expect_named(mutate(df, z = 1), c("x", "y", "z"))
  expect_named(mutate(df, z = 1, .before = x), c("z", "x", "y"))
  expect_named(mutate(df, z = 1, .after = x), c("x", "z", "y"))

  # but doesn't affect order of existing columns
  df <- tidytable(x = 1, y = 2)
  expect_named(mutate(df, x = 1, .after = y), c("x", "y"))
})

test_that("Can use glue, #276", {
  test_df <- data.table(a = letters[1:3], b = letters[1:3])
  out <- mutate(test_df, new = glue("{a}_{b}"))
  expect_named(out, c("a", "b", "new"))
  expect_equal(as.character(out$new), c("a_a", "b_b", "c_c"))
})

test_that("Can use str_glue, #378", {
  test_df <- data.table(a = letters[1:3], b = letters[1:3])
  out <- mutate(test_df, new = str_glue("{a}_{b}"))
  expect_named(out, c("a", "b", "new"))
  expect_equal(as.character(out$new), c("a_a", "b_b", "c_c"))
  out2 <- mutate(test_df, new = paste0(str_glue("{a}_{b}_check")))
  expect_equal(as.character(out2$new), c("a_a_check", "b_b_check", "c_c_check"))
})

test_that("Can assign to the same column multiple times when .by = character(0), #332", {
  test_df <- tidytable(x = 1, y = 2)
  out <- mutate(test_df, x = x + 10, x = x, .by = character(0))
  expect_named(out, c("x", "y"))
  expect_equal(out$x, 11)
})

test_that("can use .data and .env", {
  df <- data.table(x = 1:3, y = 1:3)

  x <- 1

  col <- "x"

  df <- df %>%
    mutate(x_x = .data[[col]] + .env$x)

  expect_named(df, c("x", "y", "x_x"))
  expect_equal(df$x_x, 2:4)
})

test_that("can use .data and .env with .by", {
  df <- data.table(x = 1:3, y = c("a", "a", "b"))

  x <- 1

  df <- df %>%
    mutate(new = mean(.data$x) + .env$x, .by = y)

  expect_named(df, c("x", "y", "new"))
  expect_equal(df$new, c(2.5, 2.5, 4))
})

test_that("can use anonymous functions with map, #402", {
  df <- tidytable(x = list(1, 1, 1), y = list(2, 2, 2))

  out <- df %>%
    mutate(z = map2_dbl(x, y, function(.x, .y) .x + .y + n()))

  expect_named(out, c("x", "y", "z"))
  expect_equal(out$z, c(6, 6, 6))
})

test_that("nested across calls are handled properly, #505", {
  list_df <- tidytable(a = 1, b = 2)

  df <- tidytable(x = 1, y = list(list_df))

  res <- df %>%
    mutate(
      y = y %>%
        map(~ .x %>% mutate(across(.fns = as.character)))
    )

  expect_equal(res$y[[1]]$a, "1")
  expect_equal(res$y[[1]]$b, "2")
})

# rowwise ----------------------------------------------------------
test_that("works on rowwise_tt", {
  df <- data.table(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  res <- df %>%
    rowwise() %>%
    mutate(row_mean = mean(c(x, y)))

  expect_equal(res$row_mean, 1:3)
  expect_true(inherits(res, "rowwise_tt"))
})

test_that("can use .keep and relocate", {
  df <- data.table(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  res <- df %>%
    rowwise() %>%
    mutate(row_mean = mean(x),
           .keep = "used",
           .before = x)

  expect_named(res, c("row_mean", "x"))
  expect_equal(res$row_mean, 1:3)
})

test_that("c_across does all cols automatically", {
  df <- data.table(x = 1:3, y = 4:6)

  res <- df %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across()))
  res_check <- df %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across(everything())))

  expect_equal(res$row_mean, res_check$row_mean)
})

test_that("c_across cols selection works", {
  df <- data.table(x = 1:3, y = 4:6, z = c("a", "a", "b"))

  res <- df %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across(cols = where(is.numeric)))) %>%
    ungroup()

  expect_equal(res$row_mean, c(2.5, 3.5, 4.5))
})

test_that("c_across works with space named columns", {
  df <- data.table(`x y`= 1:3, `x z`= 1, y = 3)
  res <- df %>%
    rowwise() %>%
    mutate(sum = sum(c_across(contains(" "))))

  expect_equal(res$sum, c(2, 3, 4))
})
