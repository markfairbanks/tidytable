test_that(".before and .after relocate individual cols", {
  df <- data.table(x = 1, y = 2)
  expect_named(relocate(df, y, .before = x), c("y", "x"))
  expect_named(relocate(df, x, .after = y), c("y", "x"))
})

test_that("relocate. works", {
  df <- data.table(x = 1, y = 2)
  expect_named(relocate.(df, y, .before = x), c("y", "x"))
  expect_named(relocate.(df, x, .after = y), c("y", "x"))
})

test_that("can move blocks of variables", {
  df <- data.table(x = 1, a = "a", y = 2, b = "a")
  expect_named(relocate(df, where(is.character)), c("a", "b", "x", "y"))
})

test_that("no .before/.after moves to front", {
  df <- data.table(x = 1, y = 2)
  expect_named(relocate(df, y), c("y", "x"))
})

test_that("can only supply one of .before and .after", {
  df <- data.table(x = 1)
  expect_error(relocate(df, .before = 1, .after = 1))
})

test_that("doesn't modify-by-reference", {
  df <- data.table(x = 1, y = 2)
  relocate(df, x, .after = y)
  expect_named(df, c("x", "y"))
})

test_that("empty selection returns original df", {
  df <- data.table(x = 1, y = 2)
  expect_named(relocate(df), c("x", "y"))
})

test_that("multiple before/after works", {
  df <- data.table(x = 1, y = 2, z = 3)
  expect_named(relocate(df, x, .after = c(y, z)), c("y", "z", "x"))
  expect_named(relocate(df, y, .before = c(x, z)), c("y", "x", "z"))
})

test_that("can rename columns", {
  df <- data.table(x = 1, y = 2)
  res <- relocate(df, new_y = y, .before = x)
  expect_named(res, c("new_y", "x"))
  expect_equal(res$new_y, 2)
})
