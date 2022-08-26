test_that("works on all rows", {
  test_df <- tidytable(a = rep(1, 3), b = rep(2, 3), c = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct()

  expect_equal(names(test_df), names(distinct_df))
  expect_equal(distinct_df$c, c("a", "b"))
})

test_that("works on a data.frame", {
  test_df <- data.frame(a = 1:3, b = 4:6, c = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct()

  expect_equal(names(test_df), names(distinct_df))
  expect_equal(as_tidytable(test_df), distinct_df)
})

test_that("works on 1 column", {
  test_df <- data.table(a = 1:3, b = c(4,4,5), c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct(b)

  expect_named(distinct_df, c("b"))
  expect_equal(distinct_df$b, c(4, 5))
})

test_that("works on 1 column and can utilize renaming", {
  test_df <- data.table(a = rep(1, 3), b = rep(2, 3), c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct(stuff = b)

  expect_named(distinct_df, c("stuff"))
  expect_equal(distinct_df$stuff, c(2))
})

test_that("works on 1 column and can utilize renaming with .keep_all = TRUE", {
  test_df <- data.table(a = rep(1, 3), b = rep(2, 3), c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct(stuff = b, .keep_all = TRUE)

  expect_named(distinct_df, c("a", "stuff", "c", "d"))
  expect_equal(distinct_df$stuff, c(2))
})

test_that("works with enhanced selection", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct(where(is.character))

  expect_named(distinct_df, c("c", "d"))
  expect_equal(distinct_df$c, c("a", "b"))
  expect_equal(distinct_df$d, c("a", "b"))
})

test_that("can make a function with quosures", {
  test_df <- data.table(a = 1:3, b = 4:6, c = c("a", "a", "b"), d = c("a", "a", "b"))

  distinct_fn <- function(.df, col) {
    .df %>%
      distinct({{col}})
  }
  distinct_df <- test_df %>%
    distinct_fn(b)

  expect_named(distinct_df, c("b"))
  expect_equal(test_df$b, 4:6)
})

test_that("can find environment vars in custom functions, #392", {
  df <- tidytable(x = 1:3, y = c("a", "b", "a"))

  get_distinct <- function(data, char_varlist){
    var_names <- char_varlist

    data %>%
      distinct(all_of(var_names))
  }

  result_df <- df %>%
    get_distinct(char_varlist = c("y"))

  expect_named(result_df, c("y"))
  expect_equal(result_df$y, c("a", "b"))
})

test_that("distinct. works", {
  test_df <- tidytable(a = rep(1, 3), b = rep(2, 3), c = c("a", "a", "b"))
  distinct_df <- test_df %>%
    distinct.()

  expect_equal(names(test_df), names(distinct_df))
  expect_equal(distinct_df$c, c("a", "b"))
})
