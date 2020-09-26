test_that("vec_cbind returns a tidytable", {
  new_df <- vec_cbind(data.frame(x = 1:3), tidytable(y = "foo"))

  expect_true(is_tidytable(new_df))
})
