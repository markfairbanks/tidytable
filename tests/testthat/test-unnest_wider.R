# # Add back in when NULLs can be handled
# test_that("number of rows is preserved", {
#   df <- tidytable(
#     x = 1:3,
#     y = list(NULL, c(a = 1), c(a = 1, b = 2))
#   )
#   out <- df %>% unnest_wider.(y, names_sep = "_")
#   expect_equal(nrow(out), 3)
# })

test_that("list_of columns can be unnested", {
  df <- tidytable(x = 1:2, y = list_of(c(a = 1L), c(a = 1L, b = 2L)))
  expect_named(unnest_wider.(df, y), c("x", "a", "b"))

  df <- tidytable(x = 1:2, y = list_of(c(a = 1L), c(b = 1:2)))
  expect_named(unnest_wider.(df, y), c("x", "a", "b1", "b2"))
})

test_that("names_sep creates unique names", {
  df <- tidytable(
    x = list("a", c("a", "b", "c"))
  )
  out <- unnest_wider.(df, x, names_sep = "_")
  expect_named(out, c("x_1", "x_2", "x_3"))

  df <- tidytable(
    y = list(c(a = 1), c(b = 2, a = 1))
  )
  out <- unnest_wider.(df, y, names_sep = "_")
  expect_named(out, c("y_a", "y_b"))
  expect_equal(out$y_a, c(1, 1))
})
