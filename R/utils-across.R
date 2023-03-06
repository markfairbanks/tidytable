# Build across calls
expand_across <- function(call, data, .by, j, dt_env, is_top_level) {
  .fns <- call$.fns
  .cols <- get_across_cols(data, call$.cols, {{ .by }}, dt_env)
  .names <- call$.names
  dots <- call$...

  if (!is_call(.fns, c("list", "list2"))) {
    call_list <- map(.cols, ~ fn_to_expr(.fns, .x, dots, data, {{ .by }}, j, dt_env, is_top_level))

    .names <- .names %||% "{.col}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )
  } else {
    .fns <- call_args(.fns)

    is_named <- have_name(.fns)

    if (!all(is_named)) {
      names(.fns)[!is_named] <- seq_len(length(.fns))[!is_named]
    }

    fn_names <- names(.fns)

    .fns <- unname(.fns)

    call_list <- vector("list", length(.cols) * length(.fns))
    k <- 1
    for (i in seq_along(.cols)) {
      .col <- .cols[[i]]
      for (j in seq_along(.fns)) {
        call_list[[k]] <- fn_to_expr(.fns[[j]], .col, dots, data, {{ .by }}, j, dt_env, is_top_level)
        k <- k + 1
      }
    }

    .cols <- vec_rep_each(.cols, length(call_list)/length(.cols))

    fn_names <- vec_rep(fn_names, length(call_list)/length(fn_names))

    .names <- .names %||% "{.col}_{.fn}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = fn_names, col = .cols, fn = fn_names),
      repair = "check_unique", quiet = TRUE
    )
  }

  call_list
}

# Generate expression from function call
fn_to_expr <- function(.fn, .col, dots, data, .by, j, dt_env, is_top_level) {
  if (is_call(.fn, "function")) {
    .fn[[3]] <- replace_cur_column(.fn[[3]], .col)
    .fn[[3]] <- prep_expr(.fn[[3]], data, {{ .by }}, j, dt_env, is_top_level)
    call2(.fn, sym(.col), !!!dots)
  } else if (is_symbol(.fn) || is_string(.fn) || is_call(.fn, "::")) {
    call2(.fn, sym(.col), !!!dots)
  } else if (is_call(.fn, "~")) {
    call <- f_rhs(.fn)
    call <- replace_dot(call, sym(.col))
    call <- replace_cur_column(call, .col)
    prep_expr(call, data, {{ .by }}, j, dt_env, is_top_level)
  } else if (is_null(.fn)) {
    sym(.col)
  } else {
    abort(".fns needs to be a list, function name, or formula")
  }
}

# Replace occurrence of . or .x in rlang lambdas
replace_dot <- function(call, sym) {
  if (is_symbol(call, ".") || is_symbol(call, ".x")) {
    sym
  } else if (is_call(call)) {
    call[] <- lapply(call, replace_dot, sym)
    call
  } else {
    call
  }
}

# Get cols for c_across/if_all/if_any/across
# If cols is not provided defaults to everything()
# Removes .by columns from selection
get_across_cols <- function(data, .cols, .by = NULL, .env = caller_env()) {
  .cols <- .cols %||% quote(everything())
  .cols <- new_quosure(expr(c(!!.cols, - {{ .by }})), .env)
  tidyselect_names(data, !!.cols)
}

replace_cur_column <- function(x, x_name) {
  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, c("cur_column", "cur_column."))) {
    x_name
  } else {
    x[-1] <- lapply(x[-1], replace_cur_column, x_name)
    x
  }
}
