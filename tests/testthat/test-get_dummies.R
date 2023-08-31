df1 <- data.table(
  "col1"= c(letters[1:3], letters[1:3]),
  "col2" = as.factor(c(letters[3:1], letters[3:1])),
  "var1"= rnorm(6,0,1)
)

df2 <- data.table(
  "col1"= c(letters[1:3], NA, letters[1:3]),
  "col2" = as.factor(c(letters[3:1], NA, letters[3:1])),
  "var1"= rnorm(7,0,1)
)

test_that("default uses all character/factor - no NAs & no modify-by-reference", {
  dummy_df <- get_dummies(df1)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_a", "col2_b", "col2_c"))

  expect_equal(dummy_df$col1_a, c(1, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col1_b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col1_c, c(0, 0, 1, 0, 0, 1))

  expect_equal(dummy_df$col2_a, c(0, 0, 1, 0, 0, 1))
  expect_equal(dummy_df$col2_b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col2_c, c(1, 0, 0, 1, 0, 0))

  expect_named(df1, c("col1", "col2", "var1"))
})

test_that("works with data.frame input", {
  df <- as.data.frame(df1)

  dummy_df <- get_dummies(df)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_a", "col2_b", "col2_c"))

  expect_equal(dummy_df$col1_a, c(1, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col1_b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col1_c, c(0, 0, 1, 0, 0, 1))

  expect_equal(dummy_df$col2_a, c(0, 0, 1, 0, 0, 1))
  expect_equal(dummy_df$col2_b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col2_c, c(1, 0, 0, 1, 0, 0))

})

test_that("works with NAs", {
  dummy_df <- get_dummies(df2)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c", "col1_NA",
                           "col2_a", "col2_b", "col2_c", "col2_NA"))

  expect_equal(dummy_df$col1_a, c(1, 0, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col1_b, c(0, 1, 0, 0, 0, 1, 0))
  expect_equal(dummy_df$col1_c, c(0, 0, 1, 0, 0, 0, 1))
  expect_equal(dummy_df$col1_NA, c(0, 0, 0, 1, 0, 0, 0))

  expect_equal(dummy_df$col2_a, c(0, 0, 1, 0, 0, 0, 1))
  expect_equal(dummy_df$col2_b, c(0, 1, 0, 0, 0, 1, 0))
  expect_equal(dummy_df$col2_c, c(1, 0, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col2_NA, c(0, 0, 0, 1, 0, 0, 0))
})

test_that("no prefix works, even with numeric", {
  dummy_df <- df1 %>%
    mutate(col2 = c(1, 1, 1, 2, NA, 2)) %>%
    get_dummies(cols = c(col1, col2), prefix = FALSE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "a", "b", "c",
                           "1", "2", "NA"))

  expect_equal(dummy_df$a, c(1, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$c, c(0, 0, 1, 0, 0, 1))
  expect_equal(dummy_df$`1`, c(1, 1, 1, 0, 0, 0))
  expect_equal(dummy_df$`2`, c(0, 0, 0, 1, 0, 1))
  expect_equal(dummy_df$`NA`, c(0, 0, 0, 0, 1, 0))
})

test_that("prefix_sep works", {
  dummy_df <- get_dummies(df1, prefix_sep = ".")

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1.a", "col1.b", "col1.c",
                           "col2.a", "col2.b", "col2.c"))

  expect_equal(dummy_df$col1.a, c(1, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col1.b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col1.c, c(0, 0, 1, 0, 0, 1))

  expect_equal(dummy_df$col2.a, c(0, 0, 1, 0, 0, 1))
  expect_equal(dummy_df$col2.b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col2.c, c(1, 0, 0, 1, 0, 0))

})

test_that("drop_first works", {
  dummy_df <- get_dummies(df1, drop_first = TRUE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_b", "col1_c",
                           "col2_a", "col2_b"))

  expect_equal(dummy_df$col1_b, c(0, 1, 0, 0, 1, 0))
  expect_equal(dummy_df$col1_c, c(0, 0, 1, 0, 0, 1))

  expect_equal(dummy_df$col2_a, c(0, 0, 1, 0, 0, 1))
  expect_equal(dummy_df$col2_b, c(0, 1, 0, 0, 1, 0))
})

test_that("dummify_na = FALSE works", {
  dummy_df <- get_dummies(df2, dummify_na = FALSE)

  expect_named(dummy_df, c("col1", "col2", "var1",
                           "col1_a", "col1_b", "col1_c",
                           "col2_a", "col2_b", "col2_c"))

  expect_equal(dummy_df$col1_a, c(1, 0, 0, 0, 1, 0, 0))
  expect_equal(dummy_df$col1_b, c(0, 1, 0, 0, 0, 1, 0))
  expect_equal(dummy_df$col1_c, c(0, 0, 1, 0, 0, 0, 1))

  expect_equal(dummy_df$col2_a, c(0, 0, 1, 0, 0, 0, 1))
  expect_equal(dummy_df$col2_b, c(0, 1, 0, 0, 0, 1, 0))
  expect_equal(dummy_df$col2_c, c(1, 0, 0, 0, 1, 0, 0))
})
