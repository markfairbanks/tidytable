# dt starting with the j position
dt_j <- function(.df, j, ...) {
  dt(.df, , {{ j }}, ...)
}

# Create a call to `[.data.table` (j position)
call2_j <- function(.df, j = NULL, .by = NULL, .keyby = FALSE, ...) {
  .df <- enquo(.df)
  if (length(.by) == 0) {
    dt_expr <- call2("[", .df, expr(), j, ...)
  } else if (.keyby) {
    dt_expr <- call2("[", .df, expr(), j, keyby = .by, ...)
  } else {
    dt_expr <- call2("[", .df, expr(), j, by = .by, ...)
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

# Uses fast `by` trick for i position using .I
# See: https://stackoverflow.com/a/16574176/13254470
call2_i_by <- function(.df, i, .by) {
  j <- expr(.I[!!i])
  dt_expr <- call2_j(.df, j, .by)
  dt_expr <- call2("$", dt_expr, expr(V1))
  # Properly handle NA equality, #812
  dt_expr <- call2("replace_na", dt_expr, expr(FALSE), .ns = "tidytable")
  dt_expr <- call2_i(.df, dt_expr)
  dt_expr
}

globalVariables("V1")

# setnames without modify-by-reference
set_col_names <- function(.df, new_names = NULL, old_names = NULL) {
  if (is.null(old_names)) {
    out <- set_names(.df, new_names)
  } else {
    out <- fast_copy(.df)
    setnames(out, old_names, new_names)
  }
  out
}

# setcolorder without modify-by-reference
set_col_order <- function(.df, new_order) {
  out <- fast_copy(.df)
  setcolorder(out, new_order)
  out
}

# Repair names of a data.table
df_name_repair <- function(.df, .name_repair = "unique") {
  new_names <- vec_as_names(names(.df), repair = .name_repair)
  set_col_names(.df, new_names)
}

# Drop the key from a keyed data.table
remove_key <- function(.df) {
  if (haskey(.df)) {
    .df <- fast_copy(.df)
    setkey(.df, NULL)
  }
  .df
}

# Extract environment from quosures to build the evaluation environment
get_dt_env <- function(x, ...) {
  default <- caller_env(2)

  if (length(x) == 0) {
    dt_env <- default
  } else if (is_quosures(x)) {
    envs <- map(x, get_env)
    non_empty <- map_lgl(envs, ~ !identical(.x, empty_env()))
    if (any(non_empty)) {
      dt_env <- envs[non_empty][[1]]
    } else {
      dt_env <- default
    }
  } else {
    dt_env <- get_env(x)
    if (identical(dt_env, empty_env())) {
      dt_env <- default
    }
  }

  env(dt_env, ..., .datatable.aware = TRUE)
}

tidytable_class <- function() {
  c("tidytable", "tbl", "data.table", "data.frame")
}

# radix sort
# Proxy for data.table::fsort since negative values aren't supported, #282
# See: https://github.com/Rdatatable/data.table/issues/5051
f_sort <- function(x) {
  if (is.character(x)) {
    suppressWarnings(
      fsort(x, na.last = TRUE)
    )
  } else {
    vec_sort(x)
  }
}

# Unpack all data frame columns
unpack <- function(.df, .name_repair = "check_unique") {
  # Note: df_list requires data frame inputs to be unnamed to unpack
  out <- as.list(.df)
  is_data_frame <- map_lgl(out, is.data.frame)
  names(out)[is_data_frame] <- ""
  out <- df_list(!!!out, .name_repair = .name_repair)
  new_tidytable(out)
}

# imap implementation - for internal use only
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

# Is object a vector and not a matrix
is_simple_vector <- function(x) {
  is_atomic(x) && !is.matrix(x)
}

# Reduce a list of calls to a single combined call
call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}

# Restore user defined attributes
# Also ensures auto-index is removed
# See: https://github.com/Rdatatable/data.table/issues/5042
tidytable_restore <- function(x, to) {
  to <- set_attr(to, "index", NULL)
  vec_restore(x, to)
}

# Flatten lists
list_flatten <- function(x, recursive = FALSE) {
  is_list <- map_lgl(x, obj_is_list)
  any_list <- any(is_list)
  if (any_list) {
    is_not_list <- !is_list
    x[is_not_list] <- lapply(x[is_not_list], list)
    out <- list_unchop(x, ptype = list())
  } else {
    out <- x
  }

  if (recursive && any_list) {
    out <- list_flatten(out, recursive)
  }

  out
}

# Check if two vectors have compatible ptypes
vec_ptype_compatible <- function(x, y) {
  tryCatch({vec_ptype_common(x, y); TRUE}, error = function(e) FALSE)
}

check_no_across <- function(dots) {
  any_across <- any(map_lgl(dots, quo_is_call, c("across", "pick")))
  if (any_across) {
    caller_fn <- as.character(caller_call()[[1]])
    caller_fn <- str_replace(caller_fn, ".tidytable", "")
    if (any(map_lgl(dots, quo_is_call, "across"))) {
      unnecessary_fn <- "across"
    } else {
      unnecessary_fn <- "pick"
    }
    msg <- glue("`{unnecessary_fn}()` is unnecessary in `{caller_fn}()`.
                Please directly use tidyselect.
                Ex: df %>% {caller_fn}(where(is.numeric))")
    abort(msg)
  }
}

# Does type changes with ptype & transform logic
# For use in pivot_longer/unnest_longer/unnest_wider
change_types <- function(.df, .cols, .ptypes = NULL, .transform = NULL) {
  if (!is.null(.ptypes)) {
    if (!obj_is_list(.ptypes)) {
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
    if (!obj_is_list(.transform)) {
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
vec_sample <- function(x, times = vec_size(x), replace = NULL) {
  if (is.null(replace)) {
    if (vec_size(x) == times) {
      replace <- FALSE
    } else {
      replace <- TRUE
    }
  }
  locs <- vec_seq_along(x)
  locs <- sample(locs, times, replace)
  vec_slice(x, locs)
}
