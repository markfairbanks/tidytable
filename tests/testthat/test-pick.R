test_that("works", {
  df <- tidytable(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  res <- df %>%
    mutate(row_sum = rowSums(pick(where(is.numeric))))

  expect_named(res, c("x", "y", "z", "row_sum"))
  expect_equal(res$row_sum, c(2, 4, 6))
})

test_that("top level works", {
  df <- tidytable(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  res <- df %>%
    transmute(pick(where(is.numeric)))

  expect_named(res, c("x", "y"))
  expect_equal(res$x, 1:3)
})

test_that("uses `everything()` when empty", {
  df <- tidytable(x = 1:3, y = 1:3)

  res <- df %>%
    mutate(row_sum = rowSums(pick()))

  expect_named(res, c("x", "y", "row_sum"))
  expect_equal(res$row_sum, c(2, 4, 6))
})
