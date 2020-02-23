test_that(".before and .after relocate individual cols", {
  df <- data.table(x = 1, y = 2)
  expect_named(dt_relocate(df, x, .after = y), c("y", "x"))
  expect_named(dt_relocate(df, y, .before = x), c("y", "x"))
})

# # Update to pass dplyr's test?
# test_that("can move blocks of variables", {
#   df <- data.table(x = 1, a = "a", y = 2, b = "a")
#   expect_named(dt_relocate(df, is.character), c("a", "b", "x", "y"))
#   expect_named(dt_relocate(df, is.character, .after = is.numeric), c("x", "y", "a", "b"))
# })

# # Update to pass dplyr's test?
# test_that("no .before/.after moves to front", {
#   df <- data.table(x = 1, y = 2)
#   # expect_named(dt_relocate(df, y), c("y", "x"))
#   expect_error(dt_relocate(df, y), c("y", "x"))
# })

test_that("can only supply one of .before and .after", {
  df <- data.table(x = 1)
  expect_error(dt_relocate(df, .before = 1, .after = 1))
})
