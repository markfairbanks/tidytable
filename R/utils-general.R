# Creates a shallow copy
# Can add new columns or rename columns without modify-by-reference
shallow <- function(x) {
  x[TRUE]
}

# Copy columns that are being overwritten
# If columns are all new uses shallow copy
# Faster than running `copy()` on an entire dataset
fast_copy <- function(x, new_cols = character()) {
  if (length(new_cols) == 0) return(shallow(x))

  x_names <- copy(names(x))
  needs_copy <- new_cols %chin% x_names
  if (any(needs_copy)) {
    copy_cols <- new_cols[needs_copy]
  } else {
    copy_cols <- character()
  }

  if (length(copy_cols) == 0) {
    out <- shallow(x)
  } else {
    out <- vector("list", length(x_names))
    setattr(out, "names", x_names)
    for (col in x_names) {
      if (col %chin% copy_cols) {
        out[[col]] <- copy(x[[col]])
      } else {
        out[[col]] <- x[[col]]
      }
    }
    setDT(out)
    class <- copy(class(x))
    setattr(out, "class", class)
  }
  out[]
}

# dt call starting with the j position
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
setnames. <- function(x, old, new) {
  x <- shallow(x)
  setnames(x, old, new)
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
f_sort <- function(x) {
  # Can switch to data.table::fsort once negative doubles are handled
  # See: https://github.com/Rdatatable/data.table/issues/5051
  if (is.character(x)) {
    suppressWarnings(
      fsort(x)
    )
  } else {
    vec_sort(x)
  }
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
  .details <- glue("Please use `{fn}.(across.())`")

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
