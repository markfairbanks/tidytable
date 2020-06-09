# Shortcut to use rlang quoting/unquoting with data.table/base R expressions
eval_expr <- function(express) {
  eval_tidy(enexpr(express), env = caller_env())
}

eval_quo <- function(express, data = NULL, env = caller_env()) {
  eval_tidy(quo_squash(enquo(express)), data = data, env = env)
}

check_dot_by <- function(.by, by, fn) {
  if (!quo_is_null(by)) {

    if (!missing(fn))
      deprecate_warn("0.5.2", str_c("tidytable::", fn, "(by = )"), str_c(fn, "(.by = )"))

    by

  } else {
    .by
  }
}

# Creates a shallow copy to prevent modify-by-reference
shallow <- function(x, cols = names(x), reset_class = FALSE) {
  stopifnot(is.data.table(x), all(cols %in% names(x)))
  ans = vector("list", length(cols))
  setattr(ans, 'names', data.table::copy(cols))
  for (col in cols)
    ans[[col]] = x[[col]]
  setDT(ans)
  class = if (!reset_class) data.table::copy(class(x))
  else c("data.table", "data.frame")
  setattr(ans, 'class', class)
  ans[]
}
