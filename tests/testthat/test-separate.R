test_that("dt_ separate works automatically with space", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    dt_separate(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate works automatically with space", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate works automatically with comma", {
  df <- data.table(x = c("a", "a,b", "a,b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate works with sep", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"), " ")

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate can keep initial column", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"), " ", remove = FALSE)

  expect_named(df, c("x", "c1", "c2"))
  expect_equal(df$x, c("a", "a b", "a b", NA))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("separate works automatically with data.frame", {
  df <- data.frame(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})
