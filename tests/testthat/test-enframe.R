test_that("can convert unnamed vector", {
  expect_identical(
    enframe.(3:1),
    tidytable(name = 1:3, value = 3:1)
  )
})

test_that("can convert unnamed list", {
  expect_identical(
    enframe.(as.list(3:1)),
    tidytable(name = 1:3, value = as.list(3:1))
  )
})

test_that("can convert named vector", {
  expect_identical(
    enframe.(c(a = 2, b = 1)),
    tidytable(name = letters[1:2], value = as.numeric(2:1))
  )
})

test_that("can convert zero-length vector", {
  expect_identical(
    enframe.(logical()),
    tidytable(name = integer(), value = logical())
  )
})

test_that("can convert NULL (#352)", {
  expect_identical(
    enframe.(NULL),
    tidytable(name = integer(), value = logical())
  )
})

test_that("can use custom names", {
  expect_identical(
    enframe.(letters, name = "index", value = "letter"),
    tidytable(
      index = seq_along(letters),
      letter = letters
    )
  )
})

test_that("can enframe without names", {
  expect_identical(
    enframe.(letters, name = NULL, value = "letter"),
    tidytable(letter = letters)
  )
})
