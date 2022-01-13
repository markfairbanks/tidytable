test_that("work on vectors", {
  expect_equal(first.(letters), "a")
  expect_equal(last.(letters), "z")
  expect_equal(nth.(letters, 3), "c")
  expect_equal(nth.(letters, -1), "z")
  expect_equal(nth.(letters, 27), NA_character_)
  expect_equal(nth.(letters, 0), NA_character_)
})

test_that("work on data frames", {
  df <- tidytable(x = 1:3, y = 1:3)
  expect_equal(first.(df), head(df, 1))
  expect_equal(last.(df), tail(df, 1))
})
