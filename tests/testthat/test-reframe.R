test_that("works", {
  df <- tidytable(x = "x", y = "y")

  res <- df %>%
    reframe(x = rep(x, 2))

  expect_named(res, "x")
  expect_equal(res$x, c("x", "x"))
})

test_that("works with summary functions", {
  df <- tidytable(x = 1:3, y = c("a", "a", "b"))

  res <- df %>%
    reframe(mean_x = mean(x),
            .by = y)

  expect_named(res, c("y", "mean_x"))
  expect_equal(res$mean_x, c(1.5, 3))
})
