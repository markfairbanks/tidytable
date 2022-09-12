test_that("works with no wt/no .by", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(3)

  expect_equal(top_df$x, 1:3)
  expect_equal(top_df$y, 6:8)
  expect_equal(top_df$z, rep("a", 3))
})

test_that("top_n. works", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(3)

  expect_equal(top_df$x, 1:3)
  expect_equal(top_df$y, 6:8)
  expect_equal(top_df$z, rep("a", 3))
})

test_that("works with no wt, no .by, and .N", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- tidytable(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(n())

  expect_equal(top_df, test_df)
})

test_that("works with no wt/no .by with data.frame", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.frame(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)),
                        stringsAsFactors = FALSE)

  top_df <- test_df %>%
    top_n(3)

  expect_equal(top_df$x, 1:3)
  expect_equal(top_df$y, 6:8)
  expect_equal(top_df$z, rep("a", 3))
})

test_that("works with wt/no .by", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(3, wt = y)

  expect_equal(top_df$x, c(5,4,3))
  expect_equal(top_df$y, c(10,9,8))
  expect_equal(top_df$z, c("b","b","a"))
})

test_that("works with wt/no .by with data.frame", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.frame(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)),
                        stringsAsFactors = FALSE)

  top_df <- test_df %>%
    top_n(3, wt = y)

  expect_equal(top_df$x, c(5,4,3))
  expect_equal(top_df$y, c(10,9,8))
  expect_equal(top_df$z, c("b","b","a"))
})

test_that("works with no wt/with .by", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(2, .by = z)

  expect_equal(top_df$x, c(1,2,4,5))
  expect_equal(top_df$y, c(6,7,9,10))
  expect_equal(top_df$z, c("a","a","b","b"))
})

test_that("works with wt/with .by", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(2, wt = y, .by = z)

  expect_equal(top_df$x, c(5,4,3,2))
  expect_equal(top_df$y, c(10, 9, 8, 7))
  expect_equal(top_df$z, c("b","b","a","a"))
})

test_that("works with wt/with enhanced selection", {
  local_options(lifecycle_verbosity = "quiet")
  test_df <- data.table(x = 1:5,
                        y = 6:10,
                        z = c(rep("a", 3), rep("b", 2)))

  top_df <- test_df %>%
    top_n(2, wt = y, .by = where(is.character))

  expect_equal(top_df$x, c(5,4,3,2))
  expect_equal(top_df$y, c(10, 9, 8, 7))
  expect_equal(top_df$z, c("b","b","a","a"))
})
