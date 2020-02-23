test_that("joins work", {
  df1 <- data.table(a = 1:3)
  df2 <- data.table(b = 1, c = 2, a = 4:1)

  out <- dt_inner_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- dt_left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- dt_right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- dt_full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("joins work with data.frame", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1, c = 2, a = 4:1)

  out <- dt_inner_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- dt_left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- dt_right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- dt_full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})
