# dt starting with the j position
dt_j <- function(.df, j, ...) {
  j <- enquo(j)
  dt(.df, , !!j, ...)
}

# Create a call to `[.data.table` (i position)
call2_i <- function(.df, i = NULL) {
  # Use enquo(.df) to clean up error messages, #305
  call2("[", enquo(.df), i)
}

# Create a call to `[.data.table` (j position)
call2_j <- function(.df, j = NULL, .by = NULL, .keyby = NULL, ...) {
  if (is.null(.keyby)) {
    dt_expr <- call2("[", enquo(.df), , j, by = .by, ...)
  } else {
    dt_expr <- call2("[", enquo(.df), , j, keyby = .keyby, ...)
  }

  if (is_call(j, c(":=", "let"))) {
    dt_expr <- call2("[", dt_expr)
  }

  dt_expr
}

# Uses fast by trick for i position using .I
# For use in slice/filter
call2_fast_by_i <- function(.df, j, .by) {
  dt_expr <- call2_j(.df, j, .by)
  dt_expr <- call2("$", dt_expr, expr(V1))
  dt_expr <- call2_i(.df, dt_expr)
  dt_expr
}

# setnames without modify-by-reference
df_set_names <- function(.df, new_names = NULL, old_names = NULL) {
  if (is.null(old_names)) {
    names(.df) <- new_names
  } else {
    .df <- shallow(.df)
    setnames(.df, old_names, new_names)
  }
  .df
}

# setcolorder without modify-by-reference
df_col_order <- function(.df, new_order) {
  .df <- shallow(.df)
  setcolorder(.df, new_order)
  .df
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  names(.df) <- vec_as_names(
    names(.df),
    repair = .name_repair
  )
  .df
}

# Extract environment from quosures to build the evaluation environment
get_dt_env <- function(x, ...) {
  .default <- caller_env(2)
  if (length(x) == 0) {
    dt_env <- .default
  } else if (is_quosures(x)) {
    envs <- map.(x, get_env)
    non_empty <- map_lgl.(envs, ~ !identical(.x, empty_env()))
    if (any(non_empty)) {
      dt_env <- envs[non_empty][[1]]
    } else {
      dt_env <- .default
    }
  } else {
    dt_env <- get_env(x)
    if (identical(dt_env, empty_env())) {
      dt_env <- .default
    }
  }

  env(dt_env, ...)
}

# Reduce a list of calls to a single combined call
call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

# radix sort
# proxy for data.table::fsort since negative values aren't supported, #282
f_sort <- function(x) {
  # Can switch to data.table::fsort once negative doubles are handled
  # See: https://github.com/Rdatatable/data.table/issues/5051
  if (is.character(x)) {
    suppressWarnings(
      fsort(x)
    )
  } else {
    vec_sort(x, na_value = "smallest")
  }
}

# Is object a vector and not a matrix
is_simple_vector <- function(x) {
  is.atomic(x) && !is.matrix(x)
}

# Restore user defined attributes
tidytable_restore <- function(x, to) {
  # Make sure auto-index is reset since vec_restore reapplies the original index
  # https://github.com/Rdatatable/data.table/issues/5042
  attr(to, "index") <- NULL
  vec_restore(x, to)
}

deprecate_old_across <- function(fn) {
  msg <- glue("`{fn}_across.()` was deprecated in tidytable 0.6.4.
              Please use `{fn}.(across.())`")

  warn_deprecated(msg = msg, id = fn)
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

# For internal testing
sample. <- function(x, size, replace = TRUE) {
  sample(x, size, replace)
}
