# Shortcut to use rlang quoting/unquoting with data.table/base R expressions
eval_expr <- function(express) {
  eval_tidy(enexpr(express), env = caller_env())
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

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}

