# Build across calls
expand_across <- function(.fns, .cols, .names, dots) {
  if (!is_call(.fns, c("list", "list2"))) {
    call_list <- map(.cols, ~ fn_to_expr(.fns, .x, !!!dots))

    .names <- .names %||% "{.col}"

    names(call_list) <- vec_as_names(
      glue(.names, .col = .cols, .fn = "1", col = .cols, fn = "1"),
      repair = "check_unique", quiet = TRUE
    )
  } else {
    .fns <- .fns[-1]

    names_bool <- have_name(.fns)

    if (!all(names_bool)) names(.fns)[!names_bool] <- seq_len(length(.fns))[!names_bool]

    fn_names <- names(.fns)

    .args <- unname(.fns)

    call_list <- vector("list", length(.cols) * length(.args))
    k <- 1
    for (i in seq_along(.cols)) {
      .col <- .cols[[i]]
      for (j in seq_along(.args)) {
        call_list[[k]] <- fn_to_expr(.args[[j]], .col, !!!dots)
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

  call_list <- imap(call_list, replace_cur_column)

  call_list
}

# Generate expression from function call
fn_to_expr <- function(.fn, .col, ...) {
  if (is_symbol(.fn) || is_string(.fn) || is_call(.fn, c("function", "::"))) {
    call2(.fn, sym(.col), ...)
  } else if (is_call(.fn, "~")) {
    call <- f_rhs(.fn)
    call <- replace_dot(call, sym(.col))
    call
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
get_across_cols <- function(data, call_cols, .by = NULL, .env = caller_env()) {
  .cols <- call_cols %||% quote(everything())
  .cols <- new_quosure(
    expr(c(!!.cols, - {{ .by }})),
    .env
  )
  tidyselect_names(data, !!.cols)
}

replace_cur_column <- function(x, x_name) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }

  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, c("cur_column", "cur_column."))) {
    x_name
  } else {
    x[-1] <- lapply(x[-1], replace_cur_column, x_name)
    x
  }
}
