test_that("notin works", {
  expect_equal(c("a", "d") %notin% c("a", "b"), c(FALSE, TRUE))
})
