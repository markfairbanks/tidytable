# Use select_vec_*() functions when user inputs column names using c()
# Use select_dots_*() functions when user inputs column names in ...
# _i returns the index positions
# _chr returns a character vector
# _sym returns a list of symbols/expressions


## vec -----------------------------------
select_vec_i <- function(.df, select_vars) {
  select_vars <- enquo(select_vars)

  expr_char <- quo_text(select_vars)

  if (str_detect(expr_char, "list\\("))
    abort("Using by = list(col1, col2) is deprecated. Please use by = c(col1, col2)")

  eval_select(select_vars, .df)
}

select_vec_chr <- function(.df, select_vars) {
  names(select_vec_i(.df, {{ select_vars }}))
}

select_vec_sym <- function(.df, select_vars) {
  syms(select_vec_chr(.df, {{ select_vars }}))
}

select_vec_by <- function(.df, by_vars) {
  by_vars <- enquo(by_vars)

  if(quo_is_null(by_vars)) {
    by_vars <- NULL
  } else {
    by_vars <- select_vec_sym(.df, !!by_vars)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}

## dots -----------------------------------
select_dots_i <- function(.df, ...) {
  eval_select(expr(c(...)), .df)
}

select_dots_chr <- function(.df, ...) {
  names(select_dots_i(.df, ...))
}

select_dots_sym <- function(.df, ...) {
  syms(select_dots_chr(.df, ...))
}

select_dots_by <- function(.df, ...) {
  dots <- enquos(...)

  if(length(dots) == 0) {
    by_vars <- NULL
  } else {
    by_vars <- select_dots_sym(.df, ...)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}
