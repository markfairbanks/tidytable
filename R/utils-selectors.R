# vec_selector & dots_selector return a list of bare column names
# vec_selector_i & dots_selector_i return column positions/index as an integer vector
# vec_selector_by & dots_selector_by return a list() that can be unquoted in a data.table call


### Get group by cols
vec_selector_by <- function(.data, by_vars) {
  by_vars <- enexpr(by_vars)

  if(is.null(by_vars)) {
    by_vars <- NULL
  } else {
    by_vars <- vec_selector(.data, !!by_vars)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}

dots_selector_by <- function(.data, ...) {
  dots <- enexprs(...)

  if(length(dots) == 0) {
    by_vars <- NULL
  } else {
    by_vars <- dots_selector(.data, ...)

    by_vars <- expr(list(!!!by_vars))
  }
  by_vars
}

### User inputs a vector of bare column names
vec_selector <- function(.data, select_vars) {
  select_vars <- enexpr(select_vars)

  syms(names(vec_selector_i(.data, !!select_vars)))
}

vec_selector_i <- function(.data, select_vars) {
  select_vars <- enexpr(select_vars)

  expr_char <- as.character(select_vars)

  if (length(expr_char) > 1 && expr_char[[1]] == "list")
    abort("Using by = list(col1, col2) is deprecated. Please use by = c(col1, col2)")

  eval_select(select_vars, .data)
}

### User inputs bare column names using ...
dots_selector <- function(.data, ...) {
  syms(names(dots_selector_i(.data, ...)))
}

dots_selector_i <- function(.data, ...) {
  eval_select(expr(c(...)), .data)
}
