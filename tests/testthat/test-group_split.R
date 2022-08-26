test_df <- tidytable(x = 1:3, y = c("a", "a", "b"), z = c("a", "a", "b"))

test_that("keeps the grouping variables by default", {
  out <- group_split(test_df, y)

  expect_equal(out[[1]], slice(test_df, 1:2))
  expect_equal(out[[2]], slice(test_df, 3))
  expect_equal(names(out), NULL)
})

test_that("group_split. works", {
  out <- group_split.(test_df, y)

  expect_equal(out[[1]], slice(test_df, 1:2))
  expect_equal(out[[2]], slice(test_df, 3))
  expect_equal(names(out), NULL)
})

test_that("can return a named list", {
  out <- group_split(test_df, y, z, .named = TRUE)

  expect_equal(out$a_a, slice(test_df, 1:2))
  expect_equal(out$b_b, slice(test_df, 3))
})

test_that("can discard the grouping variables with .keep = FALSE", {
  out <- group_split(test_df, y, z, .keep = FALSE)
  comp_df <- select(test_df, x)

  expect_equal(out[[1]], slice(comp_df, 1:2))
  expect_equal(out[[2]], slice(comp_df, 3))
})

test_that("preserves attributes", {
  new_df <- test_df
  attr(new_df, "test") <- "foo"
  out <- group_split(new_df, y)

  expect_equal(map_chr(out, attr, "test"), rep("foo", 2))
  expect_true(all(map_lgl(out, is_tidytable)))
})

test_that("works on a grouped_tt", {
  out <- test_df %>%
    group_by(y) %>%
    group_split(y)

  expect_equal(out[[1]], slice(test_df, 1:2))
  expect_equal(out[[2]], slice(test_df, 3))
  expect_equal(names(out), NULL)
})
