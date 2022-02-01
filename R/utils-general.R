# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# Create a call to `[.data.table` (i position)
call2_i <- function(.df, i = NULL) {
  # Use enquo(.df) to clean up error messages, #305
  call2("[", enquo(.df), i)
}

# Create a call to `[.data.table` (j position)
call2_j <- function(.df, j = NULL, .by = NULL, ...) {
  dt_expr <- call2("[", enquo(.df), , j, by = .by, ...)
  call2("[", dt_expr)
}

# Uses fast by trick for i position using .I
# For use in slice/filter
call2_fast_by_i <- function(.df, j, .by) {
  dt_expr <- call2_j(.df, j, .by)
  dt_expr <- call2("$", dt_expr, expr(V1))
  dt_expr <- call2_i(.df, dt_expr)
  dt_expr
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

# Reduce a list of calls to a single combined call
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

# Restore user defined attributes
tidytable_restore <- function(x, to) {
  # Make sure auto-index is reset since vec_restore reapplies the original index
  # https://github.com/Rdatatable/data.table/issues/5042
  attr(to, "index") <- NULL
  vec_restore(x, to)
}

deprecate_old_across <- function(fn) {
  .what <- glue("tidytable::{fn}_across.()")
  .details <- glue("Please use `{fn}.(across.())")

  deprecate_warn("0.6.4", what = .what, details = .details, id = fn)
}

# Does type changes with either ptype or transform logic
# For use in pivot_longer/unnest_longer/unnest_wider
change_types <- function(.df, .to, .list, .ptypes_transform) {
  vars <- intersect(.to, names(.list))
  if (length(vars) > 0) {
    if (.ptypes_transform == "ptypes") {
      calls <- map2.(syms(vars), .list, ~ call2("vec_cast", .x, .y))
    } else if (.ptypes_transform == "transform") {
      .list <- map.(.list, as_function)
      calls <- map2.(.list, syms(vars), call2)
    } else {
      abort("Please specify ptypes or transform")
    }
    names(calls) <- vars
    .df <- mutate.(.df, !!!calls)
  }
  .df
}

# Internal mutate.(across.())
# Use when you don't want to repeat tidyselect steps
# .cols and .by should be character vectors
mutate_lapply <- function(.df, .cols, fn, ..., .by = character()) {
  if (length(.by) == 0) {
    .df <- shallow(.df)
  } else {
    copy(.df)
  }

  .df[, (.cols) := map.(.SD, fn, ...), by = .by, .SDcols = .cols]

  .df
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
