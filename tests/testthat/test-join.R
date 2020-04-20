test_that("dt_ joins work", {
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

test_that("joins work", {
  df1 <- data.table(a = 1:3)
  df2 <- data.table(b = 1, c = 2, a = 4:1)

  out <- inner_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- left_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- full_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("joins work with different names", {
  df1 <- data.table(a = 1:3)
  df2 <- data.table(b = 1, c = 2, q = 4:1)

  out <- inner_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- left_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- full_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("left joins work with different names v2", {
  df1 <- tidytable(a = c("a", "b"), b = 1:2, d = c("a", "b"))
  df2 <- tidytable(q = c("a", "b"), c = 1:2, d = c("a", "b"))

  out <- left_join.(df1, df2, by = c("a" = "q", "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- left_join.(df1, df2, by = c("a" = "q", "d" = "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- right_join.(df1, df2, by = c("a" = "q", "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- right_join.(df1, df2, by = c("a" = "q", "d" = "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

})

test_that("joins work with data.frame", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1, c = 2, a = 4:1)

  out <- inner_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- left_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- full_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})
