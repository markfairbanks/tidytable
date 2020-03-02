test_df <- data.table(
  "col1"= letters[1:3],
  "col2" = as.factor(letters[3:1]),
  "var1"= rnorm(6,0,1))

test_df2 <- data.table(
  "col1"= c(letters[1:3], NA),
  "col2" = as.factor(c(letters[3:1], NA)),
  "var1"= rnorm(7,0,1))

test_that("cols = NULL uses all cols - no NAs", {
  dummy_df <- dt_get_dummies(test_df)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_c", "col2_b", "col2_a"))
})

test_that("works with data.frame input", {
  df <- as.data.frame(test_df)

  dummy_df <- dt_get_dummies(df)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_c", "col2_b", "col2_a"))
})

test_that("cols = NULL uses all cols - with NAs", {
  dummy_df <- dt_get_dummies(test_df2)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c", "col1_NA",
                           "col2_c", "col2_b", "col2_a", "col2_NA"))
})

test_that("no prefix works", {
  dummy_df <- dt_get_dummies(test_df, cols = col1, prefix = FALSE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "a", "b", "c"))
})

test_that("prefix_sep works", {
  dummy_df <- dt_get_dummies(test_df, prefix_sep = ".")

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1.a", "col1.b", "col1.c",
                           "col2.c", "col2.b", "col2.a"))
})

test_that("drop_first works", {
  dummy_df <- dt_get_dummies(test_df, drop_first = TRUE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_b", "col1_c",
                           "col2_b", "col2_a"))
})

test_that("dummify_na = FALSE works", {
  dummy_df <- dt_get_dummies(test_df2, dummify_na = FALSE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_c", "col2_b", "col2_a"))
})
