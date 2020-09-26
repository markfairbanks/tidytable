# Use select_vec_*() functions when user inputs column names using c()
# Use select_dots_*() functions when user inputs column names in ...
# _idx: returns col index positions as a numeric vector
# _chr: returns col names as a character vector (very useful for `by` arg in data.table calls)
# _sym: returns col names as a list of symbols


## vec -----------------------------------
select_vec_idx <- function(.df, select_vars) {
  select_vars <- enquo(select_vars)

  eval_select(select_vars, .df)
}

select_vec_chr <- function(.df, select_vars) {
  names(select_vec_idx(.df, {{ select_vars }}))
}

select_vec_sym <- function(.df, select_vars) {
  syms(select_vec_chr(.df, {{ select_vars }}))
}


## dots -----------------------------------
select_dots_idx <- function(.df, ...) {
  eval_select(expr(c(...)), .df)
}

select_dots_chr <- function(.df, ...) {
  names(select_dots_idx(.df, ...))
}

select_dots_sym <- function(.df, ...) {
  syms(select_dots_chr(.df, ...))
}

## by selectors -----------------------------------
# deprecated for simplification - just use _chr
# select_vec_by <- function(.df, by_vars) {
#   by_vars <- enquo(by_vars)
#
#   if(quo_is_null(by_vars)) {
#     by_vars <- NULL
#   } else {
#     by_vars <- select_vec_sym(.df, !!by_vars)
#
#     by_vars <- expr(list(!!!by_vars))
#   }
#   by_vars
# }

# select_dots_by <- function(.df, ...) {
#   dots <- enquos(...)
#
#   if(length(dots) == 0) {
#     by_vars <- NULL
#   } else {
#     by_vars <- select_dots_sym(.df, ...)
#
#     by_vars <- expr(list(!!!by_vars))
#   }
#   by_vars
# }
