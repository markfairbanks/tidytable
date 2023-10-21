test_that("default removes old column and returns first alpha group", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, "A", regex="([[:alnum:]]+)")

  expect_named(res, "A")
  expect_equal(res$A, c("a1", "b2", "c3"))
})

test_that("can overwrite existing column without removal", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, "y", regex="([[:alnum:]]+)")

  expect_named(res, "y")
  expect_equal(res$y, c("a1", "b2", "c3"))
})

test_that("doesn't modify-by-reference", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, "A", regex="([[:alnum:]]+)")

  expect_equal(df$y, c("a1-1", "b2-2", "c3-3"))
})

test_that("match failures give NAs", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, "A", regex="(b)")
  expect_equal(res$A, c(NA, "b", NA))
})


test_that("can match multiple groups", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, c("A", "B"), regex="([[:alnum:]]+)-([[:alnum:]]+)")

  expect_equal(res$A, c("a1", "b2", "c3"))
  expect_equal(res$B, c("1", "2", "3"))
})

test_that("can drop groups", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))
  res <- df %>% extract(y, c("A", NA, "B"), regex="([[:alnum:]]+)(-)([[:alnum:]]+)")

  expect_named(res, c("A", "B"))
  expect_equal(res$B, c("1", "2", "3"))
})

test_that("can combine groups into multiple columns", {
  df <- data.table(y = "a1-1")
  res <- df %>% extract(y, c("A", "B", "B"), regex="([a-z])(\\d)-([[:alnum:]]+)")

  expect_equal(res,  tidytable(A = "a", B = "11"))
})

test_that("errors when the extracted groups number is different then the provided `into` columns", {
  df <- data.table(y = c("a1-1", "b2-2", "c3-3"))

  expect_error(df %>% extract(y, c("A", "B"), regex = "a"), "should define 2 groups")
  expect_error(df %>% extract(y, 1:3, regex = "."), "should define 3 groups")
})


test_that("extract keeps characters as character", {
  df <- data.table(x = "X-1-T")
  res <- extract(df, x, c("A", "B", "C"), "(.)-(.)-(.)", convert = TRUE)
  expect_equal(res$A, "X")
  expect_equal(res$B, 1L)
  expect_equal(res$C, TRUE)
})

test_that("str_extract_groups handles edge cases", {
  expect_identical(
    str_extract_groups(c("r-2", "d-2-3-4"), "(.)-(.)"),
    list(c("r", "d"), c("2", "2"))
  )
  expect_identical(
    str_extract_groups(NA, "test"),
    list()
  )
  expect_equal(
    str_extract_groups(c("", " "), "^(.*)$"),
    list(c("", " "))
  )
  expect_equal(
    str_extract_groups("", "(.)-(.)"),
    list(NA_character_, NA_character_)
  )
  expect_equal(
    str_extract_groups(character(), "(.)-(.)"),
    list(character(), character())
  )
})
