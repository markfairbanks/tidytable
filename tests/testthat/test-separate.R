test_that("works automatically with space", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate. works", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2")) %>%
    suppressWarnings()

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("works automatically with comma", {
  df <- data.table(x = c("a", "a,b", "a,b", NA))

  df <- df %>%
    separate(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("works with sep", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate(x, c("c1", "c2"), " ")

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a", NA))
  expect_equal(df$c2, c(NA, "b", "b", NA))
})

test_that("can keep initial column", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate(x, c("c1", "c2"), " ", remove = FALSE)

  expect_named(df, c("x", "c1", "c2"))
  expect_equal(df$x, c("a", "a b", "a b", NA))
  expect_equal(df$c1, c("a", "a", "a", NA))
  expect_equal(df$c2, c(NA, "b", "b", NA))
})

test_that("works automatically with data.frame", {
  df <- data.frame(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a", "a", "a",NA))
  expect_equal(df$c2, c(NA, "b", "b",NA))
})

test_that("can drop columns using NA, #288", {
  df <- data.frame(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate(x, c("c1", NA))

  expect_named(df, c("c1"))
  expect_equal(df$c1, c("a", "a", "a", NA))
})

test_that("fills extra columns with NA", {
  df <- tidytable(x = c("a_a", "b_b", "c_c"))

  res <- df %>%
    separate(x, c("one", "two", "three"))

  expect_named(res, c("one", "two", "three"))
  expect_equal(res$three, as.character(rep(NA_character_, 3)))
})

test_that("works when too few columns are specified", {
  df <- tidytable(x = c("a_a", "b_b", "c_c"))

  res <- df %>%
    separate(x, "one")

  expect_named(res, "one")
  expect_equal(res$one, c("a", "b", "c"))
})

test_that("can overwrite existing col, #680", {
  df <- tidytable(x = c("a_a", "b_b", "c_c"))

  res <- df %>%
    separate(x, c("x", "y"))

  expect_named(res, c("x", "y"))
  expect_equal(res$x, c("a", "b", "c"))
})
