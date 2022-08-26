test_that("returns a tidytable", {
  out <- fread(
    "A,B
    1,2
    3,4"
  )

  expect_true(is_tidytable(out))
})
