test_that("works", {
  df <- tidytable(x = "x", y = "y")

  res <- df %>%
    reframe(x = rep(x, 2))

  expect_named(res, "x")
  expect_equal(res$x, c("x", "x"))
})
