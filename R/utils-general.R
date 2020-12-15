# Shortcut to use rlang quoting/unquoting with data.table/base R expressions
# Currently used in `R/n.R` and `R/row_number.R`
# Can be replaced by rlang::inject() if rlang dependency is bumped to v0.4.9
eval_expr <- function(express) {
  eval_tidy(enexpr(express), env = caller_env())
}

# Allows use of quosures inside data.tables
# Squashes all quosures to expressions
eval_quo <- function(express, data = NULL, env = caller_env()) {
  eval_tidy(quo_squash(enquo(express)), data = data, env = env)
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

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {

  names(.df) <- vec_as_names(
    names(.df),
    repair = .name_repair
  )

  .df
}

# data.table::fsort() with no warning messages
f_sort <- function(x, decreasing = FALSE, na.last = FALSE) {
  suppressWarnings(
    fsort(x, decreasing = decreasing, na.last = na.last)
  )
}

# pmap - for internal use only
# Taken from: https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R
pmap. <- function(.l, .f, ...) {
  .f <- as_function(.f)
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

args_recycle <- function(args) {
  lengths <- map_int.(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map.(args[to_recycle], function(x) rep.int(x, n))

  args
}
