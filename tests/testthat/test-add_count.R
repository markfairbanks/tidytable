test_that("adds count", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% add_count()
  expect_equal(res$n, c(4, 4, 4, 4))

  res <- df %>% add_count(g)
  expect_equal(res$n, c(1, 3, 3, 3))
})

test_that("adds sum", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% add_count(g, wt = val)
  expect_equal(res$n, c(1, 6, 6, 6))
})

test_that("works on a grouped_tt", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>%
    group_by(g) %>%
    add_count()
  expect_equal(res$n, c(1, 3, 3, 3))
})

test_that("add_count. works", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% add_count.() %>% suppressWarnings()
  expect_equal(res$n, c(4, 4, 4, 4))
})

# add_tally() ----------------------------------------------------
test_that("adds count", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% add_tally()
  expect_equal(res$n, c(4, 4, 4, 4))
})

test_that("adds sum and works on grouped_tt", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% group_by(g) %>% add_tally(wt = val)
  expect_equal(res$n, c(1, 6, 6, 6))
})

test_that("add_tally. works", {
  df <- tidytable(g = c(1, 2, 2, 2), val = c(1, 1, 2, 3))
  res <- df %>% add_tally.() %>% suppressWarnings()
  expect_equal(res$n, c(4, 4, 4, 4))
})
