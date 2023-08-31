test_that("uses input for default column names", {
  df <- tidytable(x = 1:2, y = list(1, 1:2))
  out <- df %>% unnest_longer(y)

  expect_named(out, c("x", "y"))
})

test_that("automatically adds id col if named", {
  df <- tidytable(x = 1:2, y = list(c(a = 1), c(b = 2)))
  out <- df %>% unnest_longer(y)

  expect_named(out, c("x", "y", "y_id"))
})

test_that("can force integer indexes", {
  df <- tidytable(x = 1:2, y = list(1, 2))
  out <- df %>% unnest_longer(y, indices_include = TRUE)
  expect_named(out, c("x", "y", "y_id"))

  out <- df %>% unnest_longer(y, indices_to = "y2")
  expect_named(out, c("x", "y", "y2"))
})

test_that("drop empty rows when `keep_empty = FALSE`", {
  df <- tidytable(
    x = 1:3,
    y = list(NULL, NULL, 1)
  )
  out <- df %>% unnest_longer(y)
  expect_equal(nrow(out), 1)
})

test_that("preserves empty rows when `keep_empty = TRUE`", {
  df <- data.table(
    x = 1:3,
    y = list(NULL, NULL, 1)
  )
  out <- df %>% unnest_longer(y, keep_empty = TRUE)
  expect_equal(nrow(out), 3)
})

# test_that("can handle data frames consistently with vectors" , {
#   df <- tibble(x = 1:2, y = list(tibble(a = 1:2, b = 2:3)))
#   out <- df %>% unnest_longer(y)
#
#   expect_named(out, c("x", "y"))
#   expect_equal(nrow(out), 4)
# })

test_that("can unnest dates", {
  x <- as.Date(c("2019-08-01", "2019-12-01"))
  df <- tidytable(x = as.list(x))
  out <- df %>% unnest_longer(x)
  expect_equal(out$x, x)
})

test_that("bad inputs generate errors", {
  df <- tidytable(x = 1, y = list(mean))
  expect_error(unnest_longer(df, y))
})

test_that("ptype and transform work", {
  df <- tidytable(
    x = 1:3,
    y = list(0, 1:3, 4:5)
  )

  ptype_df <- df %>%
    unnest_longer(y, ptype = list(y = int()))

  expect_true(is.integer(ptype_df$y))

  transform_df <- df %>%
    unnest_longer(y, transform = list(y = as.integer))

  expect_true(is.integer(transform_df$y))
})




