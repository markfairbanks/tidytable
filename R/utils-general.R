# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# Create a call to data.table subset "[" (i position)
call2_i <- function(data, i = NULL) {
  call2("[", data, i)
}

# Create a call to data.table subset "[" (j position)
call2_j <- function(data, j = NULL, .by = NULL, ...) {
  dt_expr <- call2("[", data, , j, by = .by, ...)
  call2("[", dt_expr)
}

# Call a data.table function
# Squashes quosures
call2_dt <- function(.fn, ..., .ns = "data.table") {
  call <- call2(.fn, ..., .ns = .ns)
  quo_squash(call)
}

# Extract environment from quosures to build the evaluation environment
get_dt_env <- function(x, ...) {
  if (length(x) == 0) {
    dt_env <- caller_env(2)
  } else if (is_quosures(x)) {
    x <- x[[1]]
    dt_env <- get_env(x)
  } else {
    dt_env <- get_env(x)
  }

  if (identical(dt_env, empty_env())) {
    dt_env <- caller_env(2)
  }

  env(dt_env, ...)
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  names(.df) <- vec_as_names(
    names(.df),
    repair = .name_repair
  )
  .df
}

# Allows use of quosures inside data.tables
# Squashes all quosures to expressions
eval_quo <- function(express, data = NULL, env = caller_env()) {
  eval_tidy(quo_squash(enquo(express)), data = data, env = env)
}

# Reduce a list of calls
call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

# radix sort
# proxy for data.table::fsort since negative values aren't supported, #282
f_sort <- function(x, decreasing = FALSE, na.last = FALSE) {
  # # Can switch to data.table::fsort once negative doubles are handled
  # suppressWarnings(
  #   fsort(x, decreasing = decreasing, na.last = na.last)
  # )

  sort(x, decreasing = decreasing, na.last = na.last, method = "radix")
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

tidytable_restore <- function(x, to) {
  # Make sure auto-index is reset since vec_restore reapplies the original index
  # https://github.com/Rdatatable/data.table/issues/5042
  attr(to, "index") <- NULL
  vec_restore(x, to)
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
