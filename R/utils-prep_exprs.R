# "Prepare" quosures/expressions for use in a `[.data.table` call
#   Allows the use of functions like n() and across()
#   Replaces these functions with the necessary data.table translations
#   Adapted from dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
prep_exprs <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_level = FALSE) {
  x <- lapply(x, prep_expr, data, {{ .by }}, j = j, dt_env = dt_env, TRUE)
  squash(x)
}

prep_expr <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_level = FALSE) {
  if (is_quosure(x)) {
    x <- get_expr(x)
  }

  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, tidytable_fns)) {
    # Ignore nested calls to tidytable functions, #505
    x
  } else if (is_call(x, call_fns)) {
    prep_expr_call(x, data, {{ .by }}, j, dt_env, is_top_level)
  } else {
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j = j, dt_env)
    x
  }
}

tidytable_fns <- c(
  "arrange.", "arrange",
  "filter.", "filter",
  "mutate.", "mutate",
  "slice.", "slice",
  "summarise.", "summarise", "summarize.", "summarize"
)

call_fns <- c(
  "$", "[[",
  "across.", "across",
  "c_across.", "c_across",
  "cur_data.", "cur_data",
  "cur_group_rows.", "cur_group_rows",
  "cur_group_id.", "cur_group_id",
  "desc.", "desc",
  "function",
  "glue",
  "if_all.",  "if_all",
  "if_any.", "if_any",
  "ifelse",
  "n.", "n",
  "pick",
  "row_number.", "row_number",
  "str_glue"
)

prep_expr_call <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_level) {
  if (is_call(x, c("n.", "n"))) {
    quote(.N)
  } else if (is_call(x, c("desc.", "desc"))) {
    x[[1]] <- sym("-")
    x[[2]] <- prep_expr(x[[2]], data, {{ .by }}, j, dt_env)
    x
  } else if (is_call(x, c("row_number.", "row_number", "cur_group_rows.", "cur_group_rows"), 0)) {
    quote(seq_len(.N))
  } else if (is_call(x, c("cur_group_id.", "cur_group_id"))) {
    quote(.GRP)
  } else if (is_call(x, c("cur_data.", "cur_data"))) {
    quote(.SD)
  } else if (is_call(x, "ifelse", ns = "")) {
    x <- call_match(x, base::ifelse)
    x <- unname(x)
    x[[1]] <- quote(tidytable::if_else)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j, dt_env)
    x
  } else if (is_call(x, c("c_across.", "c_across"))) {
    call <- call_match(x, tidytable::c_across.)
    cols <- get_across_cols(data, call$cols, {{ .by }}, dt_env)
    cols <- syms(cols)
    call2("vec_c", !!!cols, .ns = "vctrs")
  } else if (is_call(x, c("if_all.", "if_all", "if_any.", "if_any"))) {
    call <- call_match(x, tidytable::if_all)
    if (is.null(call$.fns)) {
      return(TRUE)
    }
    call_list <- expand_across(call, data, {{ .by }}, j, dt_env)
    reduce_fn <- if (is_call(x, c("if_all.", "if_all"))) "&" else "|"
    filter_expr <- call_reduce(call_list, reduce_fn)
    filter_expr
  } else if (is_call(x, c("across.", "across"))) {
    call <- call_match(x, tidytable::across, dots_expand = FALSE)
    call_list <- expand_across(call, data, {{ .by }}, j, dt_env)
    if (!is_top_level) {
      call_list <- call2("data_frame", !!!call_list, .ns = "vctrs")
    }
    call_list
  } else if (is_call(x, "pick")) {
    if (has_length(x, 1)) {
      .cols <- expr(everything())
    } else {
      .cols <- x
      .cols[[1]] <- sym("c")
    }
    call <- call2("across", .cols)
    prep_expr(call, data, {{ .by }}, j, dt_env, is_top_level)
  } else if (is_call(x, c("glue", "str_glue")) && j) {
    if (is_call(x, "str_glue")) {
      x[[1]] <- quote(glue::glue)
    }
    # Needed so the user doesn't need to specify .envir, #276
    glue_call <- call_match(x, glue::glue)
    glue_call$.envir <- glue_call$.envir %||% quote(.SD)
    glue_call
  } else if (is_data_pronoun(x)) {
    var <- x[[3]]
    if (is_call(x, "[[")) {
      var <- sym(eval(var, dt_env))
    }
    var
  } else if (is_call(x, "function")) {
    x[[3]] <- prep_expr(x[[3]], data, {{ .by }}, j, dt_env)
    x
  } else {
    # Catches case when "$" or "[[" is used but is not using .data pronoun
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j, dt_env)
    x
  }
}

is_data_pronoun <- function(x) {
  is_call(x, c("$", "[["), n = 2) && is_symbol(x[[2]], ".data")
}
