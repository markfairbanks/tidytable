test_that("default removes old column and returns first alpha group", {
  data.table(y = c("a1-1", "b2-2", "c3-3")) -> test_df
  test_df %>% extract.(y, "A", regex="([[:alnum:]]+)") -> results_df

  expect_equal(results_df$A, c("a1", "b2", "c3"))
  expect_null(results_df$y)
})

test_that("match failures give NAs", {
  data.table(y = c("a1-1", "b2-2", "c3-3")) -> test_df
  test_df %>% extract.(y, "A", regex="(b)") -> results_df
  expect_equal(results_df$A, c(NA, "b", NA))
})


test_that("can match multiple groups", {
  data.table(y = c("a1-1", "b2-2", "c3-3")) -> test_df
  test_df %>% extract.(y, c("A", "B"), regex="([[:alnum:]]+)-([[:alnum:]]+)") -> results_df

  expect_equal(results_df$A, c("a1", "b2", "c3"))
  expect_equal(results_df$B, c("1", "2", "3"))
})

test_that("can drop groups", {
  data.table(y = c("a1-1", "b2-2", "c3-3")) -> test_df
  test_df %>% extract.(y, c("A", NA, "B"), regex="([[:alnum:]]+)(-)([[:alnum:]]+)") -> results_df

  expect_named(results_df, c("A", "B"))
  expect_equal(results_df$B, c("1", "2", "3"))
})

test_that("can combine groups into multiple columns", {
  data.table(y = "a1-1") -> test_df
  test_df %>% extract.(y, c("A", "B", "B"), regex="([a-z])(\\d)-([[:alnum:]]+)") -> results_df

  expect_equal(results_df,  tidytable(A = "a", B = "11"))
})

test_that("errors when the extracted groups number is different then the provided `into` columns", {
  data.table(y = c("a1-1", "b2-2", "c3-3")) -> test_df

  expect_error(test_df %>% extract.(y, c("A", "B"), regex = "a"), "should define 2 groups")
  expect_error(test_df %>% extract.(y, 1:3, regex = "."), "should define 3 groups")
})


test_that("extract keeps characters as character", {
  test_df <- data.table(x = "X-1-T")
  results_df <- extract.(test_df, x, c("A", "B", "C"), "(.)-(.)-(.)", convert = TRUE)
  expect_equal(results_df$A, "X")
  expect_equal(results_df$B, 1L)
  expect_equal(results_df$C, TRUE)
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
  expect_equivalent(
    str_extract_groups(c("", " "), "^(.*)$"),
    list(c("", " "))
  )
  expect_equivalent(
    str_extract_groups("", "(.)-(.)"),
    list(NA_character_, NA_character_)
  )
  expect_equivalent(
    str_extract_groups(character(), "(.)-(.)"),
    list(character(), character())
  )
})