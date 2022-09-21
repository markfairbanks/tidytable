# dt starting with the j position
dt_j <- function(.df, j, ...) {
  j <- enquo(j)
  dt(.df, , !!j, ...)
}

# Create a call to `[.data.table` (j position)
call2_j <- function(.df, j = NULL, .by = NULL, .keyby = FALSE, ...) {
  if (length(.by) == 0) {
    dt_expr <- call2("[", enquo(.df), , j, ...)
  } else if (.keyby) {
    dt_expr <- call2("[", enquo(.df), , j, keyby = .by, ...)
  } else {
    dt_expr <- call2("[", enquo(.df), , j, by = .by, ...)
  }

  if (is_call(j, c(":=", "let"))) {
    dt_expr <- call2("[", dt_expr)
  }

  dt_expr
}

# Create a call to `[.data.table` (i position)
call2_i <- function(.df, i = NULL, .by = NULL) {
  if (length(.by) == 0) {
    # Use enquo(.df) to clean up error messages, #305
    call2("[", enquo(.df), i)
  } else {
    call2_i_by(.df, i, .by)
  }
}

# Uses fast by trick for i position using .I
# For use in slice/filter
call2_i_by <- function(.df, i, .by) {
  j <- expr(.I[!!i])
  dt_expr <- call2_j(.df, j, .by)
  dt_expr <- call2("$", dt_expr, expr(V1))
  dt_expr <- call2_i(.df, dt_expr)
  dt_expr
}

globalVariables("V1")

# setnames without modify-by-reference
df_set_names <- function(.df, new_names = NULL, old_names = NULL) {
  if (is.null(old_names)) {
    out <- set_names(.df, new_names)
  } else {
    out <- fast_copy(.df)
    setnames(out, old_names, new_names)
  }
  out
}

# setcolorder without modify-by-reference
df_col_order <- function(.df, new_order) {
  out <- fast_copy(.df)
  setcolorder(out, new_order)
  out
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  new_names <- vec_as_names(names(.df), repair = .name_repair)
  df_set_names(.df, new_names)
}

check_by <- function(.by) {
  .by <- enquo(.by)
  if (!quo_is_null(.by)) {
    msg <- "`.by` cannot be used on a grouped tidytable. Please `ungroup()` your data."
    stop(msg, call. = FALSE)
  }
}

# Extract environment from quosures to build the evaluation environment
get_dt_env <- function(x, ...) {
  .default <- caller_env(2)
  if (length(x) == 0) {
    dt_env <- .default
  } else if (is_quosures(x)) {
    envs <- map(x, get_env)
    non_empty <- map_lgl(envs, ~ !identical(.x, empty_env()))
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

# Set the class attribute of an object
# Defaults to a basic tidytable
set_class <- function(x, .class = c("tidytable", "data.table", "data.frame")) {
  class(x) <- .class
  x
}

# radix sort
# proxy for data.table::fsort since negative values aren't supported, #282
f_sort <- function(x) {
  # Can switch to data.table::fsort once negative doubles are handled
  # See: https://github.com/Rdatatable/data.table/issues/5051
  if (is.character(x)) {
    suppressWarnings(
      fsort(x, na.last = TRUE)
    )
  } else {
    vec_sort(x)
  }
}

# imap implementation - for internal use only
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

# Is object a vector and not a matrix
is_simple_vector <- function(x) {
  is.atomic(x) && !is.matrix(x)
}

# Reduce a list of calls to a single combined call
call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

# Restore user defined attributes
tidytable_restore <- function(x, to) {
  # Make sure auto-index is reset since vec_restore reapplies the original index
  # https://github.com/Rdatatable/data.table/issues/5042
  attr(to, "index") <- NULL
  vec_restore(x, to)
}

check_across <- function(dots, .fn) {
  use_across <- map_lgl(dots, quo_is_call, c("across", "across."))

  if (any(use_across)) {
    abort(
      glue(
      "`across()` is unnecessary in `{.fn}()`.
      Please directly use tidyselect.
      Ex: df %>% {.fn}(where(is.numeric))")
    )
  }
}

deprecate_old_across <- function(fn) {
  msg <- glue("`{fn}_across.()` is defunct as of v0.8.1 (Aug 2022).
              It has been deprecated with warnings since v0.6.4 (Jul 2021).
              Please use `{fn}.(across.())`")

  stop_defunct(msg)
}

# Does type changes with ptype & transform logic
# For use in pivot_longer/unnest_longer/unnest_wider
change_types <- function(.df, .cols, .ptypes = NULL, .transform = NULL) {
  if (!is.null(.ptypes)) {
    if (!vec_is_list(.ptypes)) {
      # Allow providing a single ptype for all cols
      .ptypes <- vec_rep(list(.ptypes), length(.cols))
      .ptypes <- set_names(.ptypes, .cols)
    }
    .cols <- intersect(.cols, names(.ptypes))
    ptype_exprs <- map2(syms(.cols), .ptypes, ~ call2("vec_cast", .x, .y, .ns = "vctrs"))
    names(ptype_exprs) <- .cols
    .df <- mutate(.df, !!!ptype_exprs)
  }

  if (!is.null(.transform)) {
    if (!vec_is_list(.transform)) {
      # Allow providing a single transform for all cols
      .transform <- vec_rep(list(.transform), length(.cols))
      .transform <- set_names(.transform, .cols)
    }
    .cols <- intersect(.cols, names(.transform))
    .transform <- map(.transform, as_function)
    transform_exprs <- map2(.transform, syms(.cols), call2)
    names(transform_exprs) <- .cols
    .df <- mutate(.df, !!!transform_exprs)
  }

  .df
}

# For internal testing
sample. <- function(x, size, replace = TRUE) {
  sample(x, size, replace)
}
