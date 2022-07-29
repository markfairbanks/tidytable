test_that("_across are defunct", {
  df <- tidytable(x = 1:3)
  expect_error(arrange_across.(df))
  expect_error(mutate_across.(df, everything(), identity))
  expect_error(summarize_across.(df, everything(), mean))
})

# test_that("across deprecations", {
#   test_df <- tidytable(a = c("a", "a", "b"), b = 1:3)
#
#   expect_warning(arrange_across.(test_df))
#   expect_warning(mutate_across.(test_df, everything(), identity))
#   expect_warning(summarize_across.(test_df, b, mean))
# })
#
# test_df <- tidytable(a = c("a", "b", "a"), b = 3:1)
#
# test_that("ascending order works", {
#   across_df <- suppressWarnings(
#     arrange_across.(test_df)
#   )
#
#   check_df <- arrange.(test_df, a, b)
#
#   expect_equal(across_df, check_df)
#
# })
#
# test_that("descending order works", {
#   across_df <- arrange_across.(test_df, everything(), desc.)
#
#   check_df <- arrange.(test_df, -a, -b)
#
#   expect_equal(across_df, check_df)
# })
#
# test_that("descending order works with rlang lambda", {
#   across_df <- arrange_across.(test_df, everything(), ~ desc.(.x))
#
#   check_df <- arrange.(test_df, -a, -b)
#
#   expect_equal(across_df, check_df)
# })
#
# test_that(".cols works with is.numeric", {
#   df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(where(is.numeric), function(.x) .x + 1) %>%
#     suppressWarnings()
#
#   expect_equal(df$x_start, c(2,2,2))
#   expect_equal(df$end_x, c(3,3,3))
# })
#
# test_that("modify-by-reference doesn't occur", {
#   df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
#   df %>%
#     mutate_across.(where(is.numeric), ~ .x + 1)
#
#   df %>%
#     mutate_across.(where(is.numeric), ~ 1, .by = z)
#
#   expect_equal(df$x_start, c(1,1,1))
#   expect_equal(df$end_x, c(2,2,2))
# })
#
# test_that(".cols works with is.numeric with data.frame", {
#   df <- data.frame(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(where(is.numeric), function(.x) .x + 1)
#
#   expect_equal(df$x_start, c(2,2,2))
#   expect_equal(df$end_x, c(3,3,3))
# })
#
# test_that(".cols works with is.character", {
#   df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(where(is.character), function(.x) paste0(.x, "_append"))
#
#   expect_equal(df$x_start, c(1,1,1))
#   expect_equal(df$z, c("a_append", "a_append", "b_append"))
# })
#
# test_that("works with newly named columns", {
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), list(new = function(.x) .x + 1))
#
#   expect_named(df, c("x","y","z","x_new","y_new"))
#   expect_equal(df$x_new, c(2,2,2))
#   expect_equal(df$y_new, c(3,3,3))
# })
#
# test_that("works with newly named columns using .names with single .fn", {
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), ~ .x + 1, .names = "new_{.col}")
#
#   expect_named(df, c("x","y","z","new_x","new_y"))
#   expect_equal(df$new_x, c(2,2,2))
#   expect_equal(df$new_y, c(3,3,3))
# })
#
# test_that("works with newly named columns using .names with single .fn using col", {
#   # This test will need to be removed when {col} is deprecated
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), ~ .x + 1, .names = "new_{col}")
#
#   expect_named(df, c("x","y","z","new_x","new_y"))
#   expect_equal(df$new_x, c(2,2,2))
#   expect_equal(df$new_y, c(3,3,3))
# })
#
# test_that("works with newly named columns using .names", {
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), list(new = function(.x) .x + 1), .names = "{.fn}_{.col}")
#
#   expect_named(df, c("x","y","z","new_x","new_y"))
#   expect_equal(df$new_x, c(2,2,2))
#   expect_equal(df$new_y, c(3,3,3))
# })
#
# test_that("works with newly named columns using .names with using fn and col", {
#   # This test will need to be removed when {col} and {fn} is deprecated
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), list(new = function(.x) .x + 1), .names = "{fn}_{col}")
#
#   expect_named(df, c("x","y","z","new_x","new_y"))
#   expect_equal(df$new_x, c(2,2,2))
#   expect_equal(df$new_y, c(3,3,3))
# })
#
# test_that("works with newly named columns using .names w/ autonaming", {
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   df <- df %>%
#     mutate_across.(c(x:y), list(new = ~ .x + 1, ~ .x + 2), .names = "{.col}_{.fn}_stuff")
#
#   expect_named(df, c("x","y","z","x_new_stuff", "x_2_stuff","y_new_stuff", "y_2_stuff"))
#   expect_equal(df$x_new_stuff, c(2,2,2))
#   expect_equal(df$y_new_stuff, c(3,3,3))
#   expect_equal(df$x_2_stuff, c(3,3,3))
#   expect_equal(df$y_2_stuff, c(4,4,4))
# })
#
# # twiddle testing ----------------------------
# test_that("works with twiddle", {
#   df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#   anon_df <- df %>%
#     mutate_across.(c(x:y), function(.x) .x + 1)
#
#   twiddle_df <- df %>%
#     mutate_across.(c(x:y), ~ .x + 1)
#
#   expect_equal(anon_df, twiddle_df)
# })
#
# test_that("works with .by, doesn't modify-by-reference", {
#   test_df <- data.table(
#     x = c(1,2,3),
#     y = c(4,5,6),
#     z = c("a","a","b"))
#
#   results_df <- test_df %>%
#     mutate_across.(c(x, y), ~ mean(.x), .by = z)
#
#   expect_named(results_df, c("x", "y", "z"))
#   expect_equal(results_df$x, c(1.5, 1.5, 3))
#   expect_equal(results_df$y, c(4.5, 4.5, 6))
#   expect_equal(test_df$x, c(1,2,3))
# })
#
# test_that(".cols doesn't use .by columns", {
#   test_df <- data.table(
#     x = c(1,2,3),
#     y = c(4,5,6),
#     z = c("a","a","b")
#   )
#
#   results_df <- test_df %>%
#     mutate_across.(everything(), ~ mean(.x), .by = z)
#
#   expect_named(results_df, c("x", "y", "z"))
#   expect_equal(results_df$x, c(1.5, 1.5, 3))
#   expect_equal(results_df$y, c(4.5, 4.5, 6))
#   expect_equal(test_df$x, c(1,2,3))
# })
#
# test_that("works with .by enhanced selection", {
#   test_df <- data.table(
#     x = c(1,2,3),
#     y = c(4,5,6),
#     z = c("a","a","b"))
#
#   results_df <- test_df %>%
#     mutate_across.(c(x, y), ~ mean(.x), .by = where(is.character))
#
#   expect_named(results_df, c("x", "y", "z"))
#   expect_equal(results_df$x, c(1.5, 1.5, 3))
#   expect_equal(results_df$y, c(4.5, 4.5, 6))
# })
#
# test_that("_across.() can refer to variables in the data.table", {
#   test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#
#   results_df <- test_df %>%
#     mutate_across.(c(x, y), ~ .x + y)
#
#   expect_equal(results_df$x, c(3,3,3))
#   expect_equal(results_df$y, c(4,4,4))
# })
#
# test_that("_across.() can refer to variables in the data.table w/ list of .fns", {
#   test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
#
#   results_df <- test_df %>%
#     mutate_across.(x, list(~ .x + 1, new = ~ .x + y))
#
#   expect_equal(results_df$x_1, c(2,2,2))
#   expect_equal(results_df$x_new, c(3,3,3))
# })
#
# test_that("_across.() can use bare functions", {
#   test_df <- tidytable(x = 1:3, y = 2:4, z = c("a", "a", "b"))
#
#   results_df <- test_df %>%
#     mutate_across.(c(x, y), between, 1, 3)
#
#   expect_equal(results_df$x, c(TRUE, TRUE, TRUE))
#   expect_equal(results_df$y, c(TRUE, TRUE, FALSE))
# })
#
# test_that("can be used in custom functions", {
#   df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
#
#   mutate_across_fn <- function(data, cols, val) {
#     data %>%
#       mutate_across.({{ cols }}, ~ .x + val)
#   }
#
#   df <- df %>%
#     mutate_across_fn(where(is.numeric), 1)
#
#   expect_equal(df$x_start, c(2,2,2))
#   expect_equal(df$end_x, c(3,3,3))
# })
#
# test_that("single function works", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), mean, na.rm = TRUE) %>%
#     suppressWarnings()
#
#   expect_named(result_df, c("a", "b"))
#   expect_equal(result_df$a, 2)
#   expect_equal(result_df$b, 5)
# })
#
# test_that("can use anonymous functions", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), ~ mean(.x, na.rm = TRUE))
#
#   expect_named(result_df, c("a", "b"))
#   expect_equal(result_df$a, 2)
#   expect_equal(result_df$b, 5)
# })
#
# test_that("can use other columns", {
#   test_df <- tidytable(a = 1:3, b = 1:3, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), ~ mean(.x)/mean(b))
#
#   expect_named(result_df, c("a", "b"))
#   expect_equal(result_df$a, 1)
#   expect_equal(result_df$b, 1)
# })
#
# test_that("summarise spelling works", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarise_across.(c(a, b), mean, na.rm = TRUE)
#
#   expect_named(result_df, c("a", "b"))
#   expect_equal(result_df$a, 2)
#   expect_equal(result_df$b, 5)
# })
#
# test_that("single function works with .by", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), mean, na.rm = TRUE, .by = z)
#
#   expect_named(result_df, c("z", "a", "b"))
#   expect_equal(result_df$a, c(1.5, 3))
#   expect_equal(result_df$b, c(4.5, 6))
# })
#
# test_that(".cols doesn't use .by columns", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(everything(), mean, na.rm = TRUE, .by = z)
#
#   expect_named(result_df, c("z", "a", "b"))
#   expect_equal(result_df$a, c(1.5, 3))
#   expect_equal(result_df$b, c(4.5, 6))
# })
#
# test_that("can pass list of named functions", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(avg = mean, max = max))
#
#   expect_named(result_df, c("a_avg", "a_max", "b_avg", "b_max"))
#   expect_equal(result_df$a_avg, 2)
#   expect_equal(result_df$b_avg, 5)
#   expect_equal(result_df$a_max, 3)
#   expect_equal(result_df$b_max, 6)
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(avg = ~ mean(.x), max = ~ max(.x)))
#
#   expect_named(result_df, c("a_avg", "a_max", "b_avg", "b_max"))
#   expect_equal(result_df$a_avg, 2)
#   expect_equal(result_df$b_avg, 5)
#   expect_equal(result_df$a_max, 3)
#   expect_equal(result_df$b_max, 6)
# })
#
# test_that("can pass list of functions with no names", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(mean, max))
#
#   expect_named(result_df, c("a_1", "a_2", "b_1", "b_2"))
#   expect_equal(result_df$a_1, 2)
#   expect_equal(result_df$b_1, 5)
#   expect_equal(result_df$a_2, 3)
#   expect_equal(result_df$b_2, 6)
# })
#
# test_that("can pass list of functions with some names", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(avg = mean, max))
#
#   expect_named(result_df, c("a_avg", "a_2", "b_avg", "b_2"))
#   expect_equal(result_df$a_avg, 2)
#   expect_equal(result_df$b_avg, 5)
#   expect_equal(result_df$a_2, 3)
#   expect_equal(result_df$b_2, 6)
# })
#
# test_that("can pass list of named functions with .by and .names", {
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(avg = mean, max = max), .by = z, .names = "{.fn}_{.col}")
#
#   expect_named(result_df, c("z", "avg_a", "max_a", "avg_b", "max_b"))
#   expect_equal(result_df$avg_a, c(1.5, 3))
#   expect_equal(result_df$avg_b, c(4.5, 6))
#   expect_equal(result_df$max_a, c(2, 3))
#   expect_equal(result_df$max_b, c(5, 6))
# })
#
# test_that("can pass list of named functions with .by and .names using fn and col", {
#   # This test will need to be removed when {col} and {fn} is deprecated
#
#   test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))
#
#   result_df <- test_df %>%
#     summarize_across.(c(a, b), list(avg = mean, max = max), .by = z, .names = "{fn}_{col}")
#
#   expect_named(result_df, c("z", "avg_a", "max_a", "avg_b", "max_b"))
#   expect_equal(result_df$avg_a, c(1.5, 3))
#   expect_equal(result_df$avg_b, c(4.5, 6))
#   expect_equal(result_df$max_a, c(2, 3))
#   expect_equal(result_df$max_b, c(5, 6))
# })
