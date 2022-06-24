# arrange -----------------------------------------------------
test_df <- tidytable(a = c("a", "b", "a"), b = 3:1)

test_that("ascending order works", {
  across_df <- arrange.(test_df, across.())

  check_df <- arrange.(test_df, a, b)

  expect_equal(across_df, check_df)
})

test_that("descending order works with desc.", {
  across_df <- arrange.(test_df, across.(everything(), desc.))

  check_df <- arrange.(test_df, -a, -b)

  expect_equal(across_df, check_df)
})

test_that("descending order works with desc", {
  across_df <- arrange.(test_df, across.(everything(), desc))

  check_df <- arrange.(test_df, -a, -b)

  expect_equal(across_df, check_df)
})

test_that("descending order works with rlang lambda", {
  across_df <- arrange.(test_df, across.(everything(), ~ desc.(.x)))

  check_df <- arrange.(test_df, -a, -b)

  expect_equal(across_df, check_df)
})

# mutate -----------------------------------------------------
test_that(".cols selection works", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate.(across.(where(is.numeric), function(.x) .x + 1))

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("works with newly named columns & allows other transforms", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate.(
      across.(c(x:y), list(new = ~ .x + 1)),
      double_x = x * 2
    )

  expect_named(df, c("x","y","z","x_new","y_new", "double_x"))
  expect_equal(df$x_new, c(2,2,2))
  expect_equal(df$y_new, c(3,3,3))
})

test_that("works with newly named columns using .names with single .fn", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate.(across.(c(x:y), ~ .x + 1, .names = "new_{.col}"))

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("works with newly named columns using .names", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate.(across.(c(x:y), list(new = function(.x) .x + 1), .names = "{.fn}_{.col}"))

  expect_named(df, c("x","y","z","new_x","new_y"))
  expect_equal(df$new_x, c(2,2,2))
  expect_equal(df$new_y, c(3,3,3))
})

test_that("works with newly named columns using .names w/ autonaming", {
  df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))
  df <- df %>%
    mutate.(across.(c(x:y), list(new = ~ .x + 1, ~ .x + 2), .names = "{.col}_{.fn}_stuff"))

  expect_named(df, c("x","y","z","x_new_stuff", "x_2_stuff","y_new_stuff", "y_2_stuff"))
  expect_equal(df$x_new_stuff, c(2,2,2))
  expect_equal(df$y_new_stuff, c(3,3,3))
  expect_equal(df$x_2_stuff, c(3,3,3))
  expect_equal(df$y_2_stuff, c(4,4,4))
})

test_that(".cols doesn't use .by columns", {
  test_df <- data.table(
    x = c(1,2,3),
    y = c(4,5,6),
    z = c("a","a","b")
  )

  results_df <- test_df %>%
    mutate.(across.(everything(), ~ mean(.x)), .by = z)

  expect_named(results_df, c("x", "y", "z"))
  expect_equal(results_df$x, c(1.5, 1.5, 3))
  expect_equal(results_df$y, c(4.5, 4.5, 6))
  expect_equal(test_df$x, c(1,2,3))
})

test_that("can refer to variables in the data.table", {
  test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate.(across.(c(x, y), ~ .x + y))

  expect_equal(results_df$x, c(3,3,3))
  expect_equal(results_df$y, c(4,4,4))
})

test_that("can refer to variables in the data.table w/ list of .fns", {
  test_df <- data.table(x = c(1,1,1), y = c(2,2,2), z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate.(across.(x, list(~ .x + 1, new = ~ .x + y)))

  expect_equal(results_df$x_1, c(2,2,2))
  expect_equal(results_df$x_new, c(3,3,3))
})

test_that("can use bare functions", {
  test_df <- tidytable(x = 1:3, y = 2:4, z = c("a", "a", "b"))

  results_df <- test_df %>%
    mutate.(across.(c(x, y), between, 1, 3))

  expect_equal(results_df$x, c(TRUE, TRUE, TRUE))
  expect_equal(results_df$y, c(TRUE, TRUE, FALSE))
})

test_that("can be used in custom functions", {
  df <- data.table(x_start = c(1,1,1), end_x = c(2,2,2), z = c("a", "a", "b"))

  mutate_across_fn <- function(data, cols, val) {
    data %>%
      mutate.(across.({{ cols }}, ~ .x + val))
  }

  df <- df %>%
    mutate_across_fn(where(is.numeric), 1)

  expect_equal(df$x_start, c(2,2,2))
  expect_equal(df$end_x, c(3,3,3))
})

test_that("can refer to newly created columns", {
  test_df <- tidytable(x = rep(1, 3), y = rep(2, 3))

  out <- test_df %>%
    mutate.(
      double_x = x * 2,
      across.(c(x, double_x), as.character)
    )

  expect_equal(out$double_x, c("2", "2", "2"))
})

test_that("tidyselect with no cols works, #280", {
  df <- tidytable(x = 1:3, y = 1:3)

  across_df <- df %>%
    mutate.(
      across.(starts_with("a"), ~ .x + 1)
    )

  expect_equal(df, across_df)
})

test_that("works with rowSums, #346/352", {
  df <- tidytable(x = 1:3, y = 1:3, z = c("a", "a", "b"))

  across_df <- df %>%
    mutate.(row_sum = rowSums(across.(where(is.numeric))))
  expect_named(across_df, c("x", "y", "z", "row_sum"))
  expect_equal(across_df$row_sum, c(2, 4, 6))

  across_df2 <- df %>%
    mutate.(rowSums(across.(where(is.numeric))))

  expect_equal(across_df2[[4]], c(2, 4, 6))
})

test_that(".cols detects environment variables in custom functions, #389", {
  df <- tidytable(x = 1:3, y = 1:3, z = 1:3)

  make_character <- function(data, char_varlist){
    var_names <- char_varlist
    data %>%
      mutate.(across.(.cols = all_of(var_names), .fns = ~as.character(.)))
  }

  out <- df %>%
    make_character(char_varlist = c("x", "y"))

  expect_equal(out$x, c("1", "2", "3"))
  expect_equal(out$y, c("1", "2", "3"))
})

test_that("can namespace functions, #511", {
  df <- tidytable(x = 1:3, y = 1:3)

  res <- df %>%
    summarize.(across.(.fns = base::mean))

  expect_equal(res$x, 2)
  expect_equal(res$y, 2)
})

# summarize -----------------------------------------------------
test_that("single function works", {
  test_df <- tidytable(a = c(1:2, NA), b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), mean, na.rm = TRUE))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 1.5)
  expect_equal(result_df$b, 5)
})

test_that("n works", {
  test_df <- tidytable(a = 1:3, b = 4:6)

  result_df <- test_df %>%
    summarize.(across.(c(a, b), n))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 3)
  expect_equal(result_df$b, 3)
})

test_that("can use anonymous functions", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), ~ mean(.x, na.rm = TRUE)))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 2)
  expect_equal(result_df$b, 5)
})

test_that("can use other columns", {
  test_df <- tidytable(a = 1:3, b = 1:3, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), ~ mean(.x)/mean(b)))

  expect_named(result_df, c("a", "b"))
  expect_equal(result_df$a, 1)
  expect_equal(result_df$b, 1)
})

test_that(".cols doesn't use .by columns", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(everything(), mean, na.rm = TRUE), .by = z)

  expect_named(result_df, c("z", "a", "b"))
  expect_equal(result_df$a, c(1.5, 3))
  expect_equal(result_df$b, c(4.5, 6))
})

test_that("can pass list of named functions", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), list(avg = mean, max = max)))

  expect_named(result_df, c("a_avg", "a_max", "b_avg", "b_max"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_max, 3)
  expect_equal(result_df$b_max, 6)

  result_df <- test_df %>%
    summarize.(across.(c(a, b), list(avg = ~ mean(.x), max = ~ max(.x))))

  expect_named(result_df, c("a_avg", "a_max", "b_avg", "b_max"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_max, 3)
  expect_equal(result_df$b_max, 6)
})

test_that("can pass list of functions with no names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), list(mean, max)))

  expect_named(result_df, c("a_1", "a_2", "b_1", "b_2"))
  expect_equal(result_df$a_1, 2)
  expect_equal(result_df$b_1, 5)
  expect_equal(result_df$a_2, 3)
  expect_equal(result_df$b_2, 6)
})

test_that("dots work with list of functions", {
  test_df <- tidytable(a = c(1:2, NA), b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), list(mean = mean, max = max), na.rm = TRUE))

  expect_named(result_df, c("a_mean", "a_max", "b_mean", "b_max"))
  expect_equal(result_df$a_mean, 1.5)
  expect_equal(result_df$b_mean, 5)
  expect_equal(result_df$a_max, 2)
  expect_equal(result_df$b_max, 6)
})

test_that("can pass list of functions with some names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(across.(c(a, b), list(avg = mean, max)))

  expect_named(result_df, c("a_avg", "a_2", "b_avg", "b_2"))
  expect_equal(result_df$a_avg, 2)
  expect_equal(result_df$b_avg, 5)
  expect_equal(result_df$a_2, 3)
  expect_equal(result_df$b_2, 6)
})

test_that("can pass list of named functions with .by and .names", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  result_df <- test_df %>%
    summarize.(
      across.(c(a, b), list(avg = mean, max = max), .names = "{.fn}_{.col}"),
      .by = z
    )

  expect_named(result_df, c("z", "avg_a", "max_a", "avg_b", "max_b"))
  expect_equal(result_df$avg_a, c(1.5, 3))
  expect_equal(result_df$avg_b, c(4.5, 6))
  expect_equal(result_df$max_a, c(2, 3))
  expect_equal(result_df$max_b, c(5, 6))
})

# distinct -----------------------------------------------------
test_that("throws and error in distinct.()", {
  test_df <- tidytable(a = 1:3, b = 4:6, z = c("a", "a", "b"))

  expect_error(distinct.(test_df, across.(everything())))
})
