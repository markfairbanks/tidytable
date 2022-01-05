test_that("works automatically with space", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("works automatically with comma", {
  df <- data.table(x = c("a", "a,b", "a,b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("works with sep", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"), " ")

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("can keep initial column", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"), " ", remove = FALSE)

  expect_named(df, c("x", "c1", "c2"))
  expect_equal(df$x, c("a", "a b", "a b", NA))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("works automatically with data.frame", {
  df <- data.frame(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", "c2"))

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})

test_that("can drop columns using NA, #288", {
  df <- data.frame(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(x, c("c1", NA))

  expect_named(df, c("c1"))
  expect_equal(df$c1, c("a","a","a",NA))
})

test_that("can use .data", {
  df <- data.table(x = c("a", "a b", "a b", NA))

  df <- df %>%
    separate.(.data$x, c("c1", "c2"), " ")

  expect_named(df, c("c1", "c2"))
  expect_equal(df$c1, c("a","a","a",NA))
  expect_equal(df$c2, c(NA,"b","b",NA))
})
