test_that("work on vectors", {
  expect_equal(first.(letters), "a")
  expect_equal(last.(letters), "z")
  expect_equal(nth.(letters, 3), "c")
  expect_equal(nth.(letters, -1), "z")

  # Returns default when out of bounds
  expect_equal(nth.(letters, 27), NA_character_)
  expect_equal(nth.(letters, 0, "test"), "test")

  # na_rm works
  expect_equal(first.(c(NA, 1), na_rm = TRUE), 1)
})

test_that("work on data frames", {
  df <- tidytable(x = 1:3, y = 1:3)
  expect_equal(first.(df), head(df, 1))
  expect_equal(last.(df), tail(df, 1))
})

test_that("work on lists", {
  l <- list(x = "x", y = "y")
  expect_equal(first.(l), "x")
  expect_equal(last.(l), "y")
  expect_equal(nth.(l, 3), NULL)
})
