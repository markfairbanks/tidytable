test_that("works", {
  res <- tribble(
    ~ x, ~ y, ~ z,
    1, 4, "a",
    2, 5, "b",
    3, 6, "c"
  )
  check <- tidytable(x = 1:3, y = 4:6, z = c("a", "b", "c"))
  expect_equal(res, check)
})
