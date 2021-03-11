test_that("can remove variables with NULL", {
  df <- data.table(x = 1:3, y = 1:3)
  tidytable_df <- mutate.(df, y = NULL)

  # check that .by with NULL works
  tidytable2_df <- mutate.(df, y = NULL, .by = x)

  df_check <- as_tidytable(df)

  expect_equal(tidytable_df, df_check[, 1])
  expect_equal(tidytable2_df, df_check[, 1])

  # if it doesn't exist
  expect_warning(df %>% mutate.(z = NULL))
})

test_that("can add multiple columns", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
  expect_equal(df$y * 2, df$double_y)
})

test_that("row_number.() works", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(row = row_number.(),
            row_check = 1:.N)

  expect_equal(df$row, df$row_check)
})

test_that("row_number.() works v2", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            row = row_number.(),
            row_check = 1:.N)

  expect_equal(df$row, df$row_check)
})

test_that("row_number() works", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(row = row_number(),
            row_check = 1:.N)

  expect_equal(df$row, df$row_check)
})

test_that("row_number() works v2", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            row = row_number(),
            row_check = 1:.N)

  expect_equal(df$row, df$row_check)
})

test_that("modify-by-reference doesn't occur", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y"))
})

test_that("modify-by-reference doesn't occur with single val", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate.(x = 1)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("modify-by-reference doesn't occur with single val variable", {
  df <- data.table(x = 1:3, y = 1:3)

  new_val <- 1

  df %>%
    mutate.(x = !!new_val)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("column order is correct", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            x = 1)

  expect_named(df, c("x", "y", "double_x"))
})

test_that("can take data.frame input", {
  df <- data.frame(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(double_x = x * 2,
            double_y = y * 2)

  expect_named(df, c("x", "y", "double_x", "double_y"))
  expect_equal(df$x * 2, df$double_x)
})

test_that("can use .by", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), .by = y)

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

test_that("can mutate in order with .by", {
  df <- tidytable(x = rep(1, 3), z = c("a", "a", "b"))

  tidytable_df <- df %>%
    mutate.(x = x + 1, y = x + 1, .by = z)

  expect_equal(tidytable_df$y, rep(3, 3))
})

test_that("modify-by-reference doesn't occur with single val with .by", {
  df <- data.table(x = 1:3, y = 1:3)
  df %>%
    mutate.(x = 1, .by = y)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(1,2,3))
})

test_that("can use .by with enhanced selection", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), .by = where(is.character))

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

test_that("can use .by with vector", {
  df <- tidytable(x = 1:5, y = c(rep("a", 4), "b"))

  tidytable_df <- df %>%
    mutate.(z = mean(x), .by = c(where(is.character)))

  datatable_df <- shallow(df)[, ':='(z = mean(x)), by = y]

  expect_equal(tidytable_df, datatable_df)

})

test_that("can use .N", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(z = .N)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(3,3,3))
})

test_that("can use .N with .by", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate.(z = .N, .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(2,2,1))
})

test_that("can use .N in existing col", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(x = .N)

  expect_named(df, c("x", "y"))
  expect_equal(df$x, c(3,3,3))
})

test_that("can use n.()", {
  df <- data.table(x = 1:3, y = 1:3)
  df <- df %>%
    mutate.(z = n.())

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(3,3,3))
})

test_that("can use n.() with by", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate.(z = n.(), .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(2,2,1))
})

test_that("can use .GRP", {
  df <- data.table(x = 1:3, y = c("a","a","b"))
  df <- df %>%
    mutate.(z = .GRP, .by = y)

  expect_named(df, c("x","y","z"))
  expect_equal(df$z, c(1,1,2))
})

# test_that("can use .y in map2.() in nested data.tables", {
#   test_df <- data.table(
#     id = seq(1, 3),
#     val_1 = seq(1, 3, 1),
#     val_2 = seq(4, 6, 1)
#   )
#
#   result_df1 <- test_df %>%
#     nest_by.(id) %>%
#     mutate.(example_1 = map2.(data, id,
#                               ~ mutate.(.x, id = .y))) %>%
#     unnest.(example_1)
#
#   expect_named(result_df1, c("id","val_1", "val_2", "id1"))
#   expect_equal(result_df1$id1, c(1,2,3))
#
#   result_df2 <- test_df %>%
#     nest_by.(id) %>%
#     mutate.(example_1 = map2.(data, id,
#                               ~ .x %>% mutate.(id = .y))) %>%
#     unnest.(example_1)
#
#   expect_named(result_df2, c("id","val_1", "val_2", "id1"))
#   expect_equal(result_df2$id1, c(1,2,3))
# })

test_that("can make custom functions with quosures", {
  df <- data.table(x = c(1,2,3), y = c(1,1,1), z = c("a","a","b"))

  add_one <- function(.data, add_col, new_name, val, by) {
    val <- val
    .data %>%
      mutate.({{ new_name }} := {{ add_col }} + val, .by = {{ by }})
  }

  result_df <- df %>%
    add_one(x, stuff, 1, z)

  expect_named(result_df, c("x", "y", "z", "stuff"))
  expect_equal(result_df$stuff, c(2,3,4))
})
