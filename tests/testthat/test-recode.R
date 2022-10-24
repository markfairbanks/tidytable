# tests borrowed from dplyr
test_that("named substitution works", {
  x1 <- letters[1:3]
  x2 <- factor(x1)

  expect_equal(recode(x1, a = "apple", .default = NA_character_), c("apple", NA, NA))
  expect_equal(recode(x2, a = "apple", .default = NA_character_), factor(c("apple", NA, NA)))
})

test_that("missing values replaced by missing argument", {
  expect_equal(recode(c(1, NA), `1` = 2), c(2, NA))
  expect_equal(recode(c(1, NA), `1` = 10, .missing = 2), c(10, 2))
})

test_that("unmatched value replaced by default argument", {
  expect_equal(recode(c(1, 2), `1` = "a", .default = "b"), c("a", "b"))
})

test_that("missing and default place nicely together", {
  expect_equal(
    recode(c(1, 2, NA), `1` = "a", .default = "b", .missing = "c"),
    c("a", "b", "c")
  )
})
