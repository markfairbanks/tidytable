test_that("can use if_all", {
  test_df <- tidytable(x = rep(1, 3), y = 1:3, z = c("a", "a", "b"))

  out <- test_df %>% filter.(if_all.(c(x, y), ~ .x < 2))

  expect_equal(out$y, 1)
})

test_that("can use with other filters", {
  test_df <- tidytable(x = rep(1, 3), y = 1:3, z = c("a", "a", "b"))

  out <- test_df %>% filter.(z == "b", if_all.(c(x, y), ~ .x < 2))

  expect_equal(nrow(out), 0)
})

test_that("can use if_any", {
  test_df <- tidytable(x = rep(1, 3), y = rep(2, 3), z = c("a", "a", "b"))

  out <- test_df %>% filter.(if_any.(c(x, y), ~ .x == 3))

  expect_equal(nrow(out), 0)
})
