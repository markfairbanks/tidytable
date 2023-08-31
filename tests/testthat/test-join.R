test_that("joins work", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(a = 4:1, c = 1, d = 2)

  out <- left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join(df1, df2 %>% filter(a <= 2), by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:4)
  expect_equal(out$b, c(1:3, NA))
})

test_that("error with no common vars", {
  df1 <- data.table(a = 1:3)
  df2 <- data.table(b = 1:3)
  expect_error(left_join(df1, df2))
})

test_that("preserve attributes", {
  df1 <- tidytable(a = 1:3)
  df2 <- tidytable(b = 1, c = 2, a = 4:1)

  attr(df1, "test") <- "foo"

  out <- left_join(df1, df2, by = "a")
  expect_equal(attr(out, "test"), "foo")
  expect_true(is_tidytable(out))
})

test_that("work with different names", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(q = 4:1, c = 1, d = 2)

  out <- left_join(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join(df1, df2 %>% filter(q <= 2), by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join(df1, df2, by = c("a" = "q"))
  expect_named(out, c("a", "b", "c", "d"))
  expect_equal(out$a, 1:4)
  expect_equal(out$b, c(1:3, NA))
})

test_that("works with keep", {
  df1 <- data.table(a = 1:3, b = 1:3)
  df2 <- data.table(q = 4:1, c = 1, d = 2)

  out <- left_join(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, 1:3)
  expect_equal(out$c, c(1, 1, 1))

  out <- right_join(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, c(NA, 3:1))
  expect_equal(out$q, 4:1)
  expect_equal(out$b, c(NA, 3:1))

  out <- inner_join(df1, df2 %>% filter(q <= 2), by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, 2:1)
  expect_equal(out$c, c(1, 1))

  out <- full_join(df1, df2, by = c("a" = "q"), keep = TRUE)
  expect_named(out, c("a", "b", "q", "c", "d"))
  expect_equal(out$a, c(1:3, NA))
  expect_equal(out$b, c(1:3, NA))
  expect_equal(out$q, 1:4)
})

test_that("joins work with different names v2", {
  df1 <- tidytable(a = c("a", "b"), b = 1:2, d = c("a", "b"))
  df2 <- tidytable(q = c("a", "b"), c = 1:2, d = c("a", "b"))

  out <- left_join(df1, df2, by = c("a" = "q", "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- left_join(df1, df2, by = c("a" = "q", "d" = "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- right_join(df1, df2, by = c("a" = "q", "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

  out <- right_join(df1, df2, by = c("a" = "q", "d" = "d"))
  expect_named(out, c("a", "b", "d", "c"))
  expect_equal(out$a, c("a", "b"))
  expect_equal(out$b, 1:2)
  expect_equal(out$d, c("a", "b"))
  expect_equal(out$c, 1:2)

})

test_that("joins work with data.frame", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1, c = 2, a = 4:1)

  out <- inner_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 3:1)

  out <- left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 4:1)

  out <- full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("filtering joins preserve row and column order of x", {
  df1 <- data.frame(a = 4:1, b = 1)
  df2 <- data.frame(b = 1, c = 2, a = 2:3)

  out <- semi_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, 3:2)

  out <- anti_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, c(4L, 1L))
})

test_that("joins matches NAs by default", {
  df1 <- tidytable(x = c(NA_character_, 1))
  df2 <- tidytable(x = c(NA_character_, 2))

  expect_equal(nrow(semi_join(df1, df2, by = "x")), 1)
})

test_that("joins matches NAs by default", {
  df1 <- tidytable(year = 1:5, value = 1:5)
  df2 <- tidytable(year = 1:3, value = 6:8)

  left_out <- left_join(df1, df2, by = "year")
  left_check <- tidytable(year = 1:5, value.x = 1:5, value.y = c(6:8, NA, NA))
  expect_equal(left_out, left_check)

  right_out <- right_join(df1, df2, by = "year")
  right_check <- tidytable(year = 1:3, value.x = 1:3, value.y = 6:8)
  expect_equal(right_out, right_check)

  inner_out <- right_join(df1, df2, by = "year")
  inner_check <- tidytable(year = 1:3, value.x = 1:3, value.y = 6:8)
  expect_equal(inner_out, inner_check)

  full_out <- left_join(df1, df2, by = "year")
  full_check <- tidytable(year = 1:5, value.x = 1:5, value.y = c(6:8, NA, NA))
  expect_equal(full_out, full_check)
})

test_that("multiple join keys are suffixed", {
  df1 <- tidytable(a = c("a", "a", "b", "c"),
                   b = c("a", "a", "b", "c"),
                   c = 1:4)
  df2 <- data.table(a = c("a", "b"),
                    b = c("a", "b"),
                    d = 5:6)

  out <- df1 %>% left_join(df2, keep = TRUE)

  expect_named(out, c("a.x", "b.x", "c", "a.y", "b.y", "d"))
})

test_that("different order join keys are returned correctly, #495", {
  df1 <- tidytable(chr1 = c("a", "a", "b"), chr2 = c("c", "c", "d"), int1 = 1:3)
  df2 <- tidytable(chr1 = c("a", "b"), chr2 = c("c", "d"), int2 = 1:2)

  res <- df1 %>%
    left_join(df2, by = c("chr2", "chr1"))

  expect_named(res, c("chr1", "chr2", "int1", "int2"))
  expect_equal(res$chr1, df1$chr1)
  expect_equal(res$chr2, df1$chr2)
  expect_equal(res$int1, df1$int1)
})

test_that("joining on x column matched in y by col works, #625", {
  x <- tidytable(letters = c("a", "b", "c"),
                 letters2 = c("A", "B", "C"),
                 val_x = 1:3)

  y <- tidytable(letters = c("A", "B", "C"), val_y = 4:6)

  res <- left_join(x, y, by = c("letters2" = "letters"))

  expect_named(res, c("letters", "letters2", "val_x", "val_y"))
  expect_equal(res$letters, c("a", "b", "c"))
  expect_equal(res$val_y, 4:6)
})

# Some dplyr tests ----------------------------------------------------------------
test_that("when keep = TRUE, left_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3))
  expect_equal(out$x, c(NA, 3))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3))
  expect_equal(out$a.y, c(NA, 3))
})

test_that("when keep = TRUE, right_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3, NA))
  expect_equal(out$x, c(3, 4))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3, NA))
  expect_equal(out$a.y, c(3, 4))
})

test_that("when keep = TRUE, full_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3, NA))
  expect_equal(out$x, c(NA, 3, 4))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3, NA))
  expect_equal(out$a.y, c(NA, 3, 4))
})

test_that("when keep = TRUE, inner_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(x = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3))
  expect_equal(out$x, c(3))

  # when keys have same name
  df1 <- tidytable(a = c(2, 3), b = c(1, 2))
  df2 <- tidytable(a = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3))
  expect_equal(out$a.y, c(3))
})

# nest_join ----------------------------------------------------------------

test_that("nest_join works",{
  df1 <- tidytable(x = c(1, 2), y = c(2, 3))
  df2 <- tidytable(x = c(1, 1), z = c(2, 3))
  out <- nest_join(df1, df2, by = "x")

  expect_named(out, c("x", "y", "df2"))
  expect_true(is.list(out$df2))
  expect_true(is_tidytable(out$df2[[1]]))
  expect_equal(nrow(out$df2[[2]]), 0)
})

# test_that("nest_join respects types of y (#6295)",{
#   df1 <- tidytable(x = c(1, 2), y = c(2, 3))
#   df2 <- rowwise(tidytable(x = c(1, 1), z = c(2, 3)))
#   out <- nest_join(df1, df2, by = "x")
#
#   expect_s3_class(out$df2[[1]], "rowwise_df")
# })

test_that("nest_join computes common columns", {
  df1 <- tidytable(x = c(1, 2), y = c(2, 3))
  df2 <- tidytable(x = c(1, 3), z = c(2, 3))
  expect_named(nest_join(df1, df2), c("x", "y", "df2"))
})

test_that("nest_join handles multiple matches in x", {
  df1 <- tidytable(x = c(1, 1))
  df2 <- tidytable(x = 1, y = 1:2)

  out <- nest_join(df1, df2, by = "x")
  expect_equal(out$df2[[1]], out$df2[[2]])
})

# cross join ----------------------------------------------------------------
test_that("`by = character()` works",{
  df1 <- tidytable(a = c("a", "b"))
  df2 <- tidytable(b = c("c", "d"))
  cross_join_df <- tidytable(a = vctrs::vec_rep_each(c("a", "b"), 2),
                             b = vctrs::vec_rep(c("c", "d"), 2))

  expect_equal(suppressWarnings(left_join(df1, df2, character())), cross_join_df)
  expect_equal(suppressWarnings(right_join(df1, df2, character())), cross_join_df)
  expect_equal(suppressWarnings(inner_join(df1, df2, character())), cross_join_df)
  expect_equal(suppressWarnings(full_join(df1, df2, character())), cross_join_df)
  expect_equal(anti_join(df1, df2, character()), slice(df1, 0))
  expect_equal(semi_join(df1, df2, character()), df1)
})

test_that("cross_join works",{
  df1 <- tidytable(a = c("a", "b"))
  df2 <- tidytable(b = c("c", "d"))
  cross_join_df <- tidytable(a = vctrs::vec_rep_each(c("a", "b"), 2),
                             b = vctrs::vec_rep(c("c", "d"), 2))

  expect_equal(cross_join(df1, df2), cross_join_df)
})

test_that("cross_join can deal with duplicate names",{
  df1 <- tidytable(a = c("a", "b"))
  df2 <- tidytable(a = c("c", "d"))
  cross_join_df <- tidytable(a.x = vctrs::vec_rep_each(c("a", "b"), 2),
                             a.y = vctrs::vec_rep(c("c", "d"), 2))

  expect_equal(cross_join(df1, df2), cross_join_df)
})
