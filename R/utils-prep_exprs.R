# "Prepare" quosures/expressions for use in a "[.data.table" call
# Allows the use of functions like n() and across.()
# Replaces these functions with the necessary data.table translations
# Adapted from dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
prep_exprs <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_across = TRUE) {
  x <- lapply(x, prep_expr, data, {{ .by }}, j = j, dt_env = dt_env, is_top_across)
  squash(x)
}

prep_expr <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_across = TRUE) {
  if (is_quosure(x)) {
    x <- get_expr(x)
  }

  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, call_fns)) {
    prep_expr_call(x, data, {{ .by }}, j, dt_env, is_top_across)
  } else {
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j = j, dt_env = dt_env, is_top_across)
    x
  }
}

call_fns <- c(
  "$", "[[",
  "across.", "between",
  "c_across.", "case_when",
  "cur_data.", "cur_data",
  "cur_group_rows.", "cur_group_rows", "cur_group_id.", "cur_group_id",
  "desc.", "desc",
  "function",
  "glue",
  "ifelse", "if_else",
  "if_all.", "if_any.",
  "n.", "n", "row_number.", "row_number",
  "replace_na",
  "str_glue"
)

prep_expr_call <- function(x, data, .by = NULL, j = FALSE, dt_env = caller_env(), is_top_across) {
  if (is_call(x, c("n.", "n"))) {
    quote(.N)
  } else if (is_call(x, c("desc.", "desc"))) {
    x[[1]] <- sym("-")
    x[[2]] <- prep_expr(x[[2]], data, {{ .by }}, j, dt_env, is_top_across)
    x
  } else if (is_call(x, c("row_number.", "row_number", "cur_group_rows.", "cur_group_rows"))) {
    quote(1:.N)
  } else if (is_call(x, c("cur_group_id.", "cur_group_id"))) {
    quote(.GRP)
  } else if (is_call(x, c("cur_data.", "cur_data"))) {
    quote(.SD)
  } else if (is_call(x, c("ifelse", "if_else"))) {
    if (is_call(x, "if_else")) {
      x <- call_match(x, internal_if_else)
    } else {
      x <- call_match(x, base::ifelse)
    }
    x <- unname(x)
    x[[1]] <- quote(tidytable::ifelse.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j, dt_env, is_top_across)
    x
  } else if (is_call(x, "case_when", ns = "")) {
    x[[1]] <- quote(tidytable::case_when.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j, dt_env, is_top_across)
    x
  } else if (is_call(x, "replace_na")) {
    x[[1]] <- quote(tidytable::replace_na.)
    x
  } else if (is_call(x, "c_across.")) {
    call <- call_match(x, tidytable::c_across.)
    cols <- get_across_cols(data, call$cols, {{ .by }}, dt_env)
    cols <- syms(cols)
    call2("vec_c", !!!cols, .ns = "vctrs")
  } else if (is_call(x, c("if_all.", "if_any."))) {
    call <- call_match(x, tidytable::if_all.)
    if (is.null(call$.fns)) return(TRUE)
    .cols <- get_across_cols(data, call$.cols, {{ .by }}, dt_env)
    call_list <- map.(.cols, ~ fn_to_expr(call$.fns, .x))
    reduce_fn <- if (is_call(x, "if_all.")) "&" else "|"
    filter_expr <- call_reduce(call_list, reduce_fn)
    prep_expr(filter_expr, data, {{ .by }})
  } else if (is_call(x, "across.")) {
    call <- call_match(x, tidytable::across., dots_expand = FALSE)
    .cols <- get_across_cols(data, call$.cols, {{ .by }}, dt_env)
    dots <- call$...
    call_list <- expand_across(call$.fns, .cols, call$.names, dots)
    out <- lapply(call_list, prep_expr, data, {{ .by }})
    if (!is_top_across) {
      out <- call2("data_frame", !!!out, .ns = "vctrs")
    }
    out
  } else if (is_call(x, c("glue", "str_glue")) && j) {
    if (is_call(x, "str_glue")) {
      x[[1]] <- quote(glue::glue)
    }
    # Needed so the user doesn't need to specify .envir, #276
    glue_call <- call_match(x, glue::glue)
    if (is.null(glue_call$.envir)) {
      glue_call$.envir <- quote(.SD)
    }
    glue_call
  } else if (is_call(x, "between", ns = "")) {
    x[[1]] <- quote(tidytable::between.)
    x
  } else if (is_data_pronoun(x)) {
    var <- x[[3]]
    if (is_call(x, "[[")) {
      var <- sym(eval(var, dt_env))
    }
    var
  } else if (is_call(x, "function")) {
    x
  } else {
    # Catches case when "$" or "[[" is used but is not using .data pronoun
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }}, j, dt_env, is_top_across)
    x
  }
}

is_data_pronoun <- function(x) {
  is_call(x, c("$", "[["), n = 2) && is_symbol(x[[2]], ".data")
}

internal_if_else <- function(condition, true, false, missing = NULL) {
  abort("Only for use in prep_exprs")
}
