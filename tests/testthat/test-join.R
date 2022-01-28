test_that("joins work", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(a = 4:1, c = 1, d = 2)

  out <- left_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join.(df1, df2 %>% filter.(a <= 2), by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:4)
  expect_equal(out$b, c(1:3, NA))
})

test_that("preserve attributes", {
  df1 <- tidytable(a = 1:3)
  df2 <- tidytable(b = 1, c = 2, a = 4:1)

  attr(df1, "test") <- "foo"

  out <- left_join.(df1, df2, by = "a")
  expect_equal(attr(out, "test"), "foo")
  expect_true(is_tidytable(out))
})

test_that("work with different names", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(q = 4:1, c = 1, d = 2)

  out <- left_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join.(df1, df2 %>% filter.(q <= 2), by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join.(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:4)
  expect_equal(out$b, c(1:3, NA))
})

test_that("works with keep", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(q = 4:1, c = 1, d = 2)

  out <- left_join.(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join.(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, c(NA, 3:1))
  expect_equal(out$q, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join.(df1, df2 %>% filter.(q <= 2), by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join.(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, c(1:3, NA))
  expect_equal(out$b, c(1:3, NA))
  expect_equal(out$q, 1:4)
})

test_that("joins work with different names v2", {
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

test_that("filtering joins preserve row and column order of x", {
  df1 <- data.frame(a = 4:1, b = 1)
  df2 <- data.frame(b = 1, c = 2, a = 2:3)

  out <- semi_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, 3:2)

  out <- anti_join.(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, c(4L, 1L))
})

test_that("joins matches NAs by default", {
  df1 <- tidytable(x = c(NA_character_, 1))
  df2 <- tidytable(x = c(NA_character_, 2))

  expect_equal(nrow(semi_join.(df1, df2, by = "x")), 1)
})

# Some dplyr tests ----------------------------------------------------------------
test_that("when keep = TRUE, left_join.() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- left_join.(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3))
  expect_equal(out$x, c(NA, 3))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- left_join.(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3))
  expect_equal(out$a.y, c(NA, 3))
})

test_that("when keep = TRUE, right_join.() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- right_join.(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3, NA))
  expect_equal(out$x, c(3, 4))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- right_join.(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3, NA))
  expect_equal(out$a.y, c(3, 4))
})

test_that("when keep = TRUE, full_join.() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- full_join.(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3, NA))
  expect_equal(out$x, c(NA, 3, 4))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- full_join.(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3, NA))
  expect_equal(out$a.y, c(NA, 3, 4))
})

test_that("when keep = TRUE, inner_join.() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- inner_join.(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3))
  expect_equal(out$x, c(3))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- inner_join.(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3))
  expect_equal(out$a.y, c(3))
})
