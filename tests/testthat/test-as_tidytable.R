test_that("works", {
  # list
  .list <- suppressMessages(as_tidytable(list(x = 1:3, 1)))
  expect_named(.list, c("x", "...2"))
  expect_equal(.list$...2, c(1, 1, 1))
  expect_true(is_tidytable(.list))

  # data.frame
  .df <- as_tidytable(data.frame(x = 1:3, y = 1:3))
  expect_named(.df, c("x", "y"))
  expect_true(is_tidytable(.df))
  expect_true(inherits(.df, "tbl"))

  # data.table
  .dt <- as_tidytable(data.table(x = 1:3, y = 1:3))
  expect_named(.dt, c("x", "y"))
  expect_true(is_tidytable(.dt))

  # matrix & .keep_rownames
  m <- matrix(data = 1:3)
  rownames(m) <- c("a","b","c")
  .m <- as_tidytable(m, .keep_rownames = "id")
  expect_named(.m, c("id", "V1"))
  expect_true(is_tidytable(.m))
})

test_that("ungroups grouped_tt", {
  df <- tidytable(x = c("a", "a", "b")) %>%
    group_by(x)
  res <- as_tidytable(df)
  expect_false(is_grouped_df(res))
})
