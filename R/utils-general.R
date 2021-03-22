# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# Create a call to data.table subset "[" (i position)
dt_call2_i <- function(data, i = NULL, .by = NULL, ...) {
  i <- quo_squash(i)
  call2("[", data, i)
}

# Create a call to data.table subset "[" (j position)
dt_call2_j <- function(data, j = NULL, .by = NULL, ...) {
  j <- quo_squash(j)
  dt_expr <- call2("[", data, , j, by = .by, ...)
  call2("[", dt_expr)
}

# Call a data.table function
# Squashes quosures
dt_call2 <- function(.fn, ..., .ns = "data.table") {
  call <- call2(.fn, ..., .ns = .ns)
  quo_squash(call)
}

# Extract environment from quosures and build a data mask
build_data_mask <- function(x, ...) {
  if (length(x) == 0) {
    x <- quo(1)
  } else if (is_quosures(x)) {
    x <- x[[1]]
  }
  dots <- enexprs(...)
  new_data_mask(env(get_env(x), !!!dots))
}

# Allows use of functions like n()/n.() and c_across()/c_across.()
# General idea follows dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
clean_exprs <- function(x, data) {
  if (is.list(x)) {
    lapply(x, clean_expr, data)
  } else {
    clean_expr(x, data)
  }
}

clean_expr <- function(x, data) {
  if (is_quosure(x)) {
    x <- get_expr(x)
  }

  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, "n.") || is_call(x, "n")) {
    expr(.N)
  } else if (is_call(x, "desc.") || is_call(x, "desc")) {
    x[[1]] <- sym("-")
    x
  } else if (is_call(x, "row_number.") || is_call(x, "row_number")) {
    expr(1:.N)
  } else if (is_call(x, "ifelse") || is_call(x, "if_else")) {
    x[[1]] <- expr(ifelse.)
    x
  } else if (is_call(x, "case_when")) {
    x[[1]] <- expr(case_when.)
    x
  } else if (is_call(x, "replace_na")) {
    x[[1]] <- expr(replace_na.)
    x
  } else if (is_call(x, "c_across.") || is_call(x, "c_across")) {
    call <- match.call(tidytable::c_across., x, expand.dots = FALSE)
    cols <- call$cols %||% expr(everything())
    cols <- select_vec_sym(data, !!cols)
    call2("vec_c", !!!cols, .ns = "vctrs")
  } else {
    x[-1] <- lapply(x[-1], clean_expr, data)
    x
  }
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  names(.df) <- vec_as_names(
    names(.df),
    repair = .name_repair
  )

  .df
}

# Shortcut to use rlang quoting/unquoting with data.table/base R expressions
# Can be replaced by rlang::inject() if rlang dependency is bumped to v0.4.9
eval_expr <- function(express, env = caller_env()) {
  eval_tidy(enexpr(express), env = env)
}

# Allows use of quosures inside data.tables
# Squashes all quosures to expressions
eval_quo <- function(express, data = NULL, env = caller_env()) {
  eval_tidy(quo_squash(enquo(express)), data = data, env = env)
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

pmap_chr. <- function(.l, .f, ...) {
  as.character(pmap.(.l, .f, ...))
}

args_recycle <- function(args) {
  lengths <- map_int.(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map.(args[to_recycle], function(x) rep.int(x, n))

  args
}

# deprecated shallow() ------------------------------------------------
# shallow <- function(x, cols = names(x), reset_class = FALSE) {
#   stopifnot(is.data.table(x), all(cols %in% names(x)))
#   ans <- vector("list", length(cols))
#   setattr(ans, 'names', copy(cols))
#   for (col in cols) {
#     ans[[col]] = x[[col]]
#   }
#   setDT(ans)
#   class  <- if (!reset_class) copy(class(x)) else c("data.table", "data.frame")
#   setattr(ans, 'class', class)
#   ans[]
# }
