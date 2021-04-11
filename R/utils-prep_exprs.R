# "Prepare" quosures/expressions for use in a data.table "[" call
# Allows use of functions like n()/n.() and c_across()/c_across.()
  ## Replaces these functions with the necessary data.table translations
# General idea follows dt_squash found here: https://github.com/tidyverse/dtplyr/blob/master/R/tidyeval.R
prep_exprs <- function(x, data, .by = NULL) {
  if (is.list(x)) {
    x <- lapply(x, prep_expr, data, {{ .by }})
    squash(x)
  } else {
    prep_expr(x, data, {{ .by }})
  }
}

prep_expr <- function(x, data, .by = NULL) {
  if (is_quosure(x)) {
    x <- get_expr(x)
  }

  if (is_symbol(x) || is_atomic(x) || is_null(x)) {
    x
  } else if (is_call(x, c("n.", "n"))) {
    quote(.N)
  } else if (is_call(x, c("desc.", "desc"))) {
    x[[1]] <- sym("-")
    x[[2]] <- get_expr(x[[2]])
    x
  } else if (is_call(x, c("row_number.", "row_number"))) {
    quote(1:.N)
  } else if (is_call(x, c("ifelse", "if_else"))) {
    if (is_call(x, "if_else")) {
      x <- match.call(internal_if_else, x)
    } else {
      x <- match.call(base::ifelse, x)
    }
    x <- unname(x)
    x[[1]] <- quote(ifelse.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  } else if (is_call(x, "case_when")) {
    x[[1]] <- quote(case_when.)
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  } else if (is_call(x, "replace_na")) {
    x[[1]] <- quote(replace_na.)
    x
  } else if (is_call(x, "c_across.")) {
    call <- match.call(tidytable::c_across., x, expand.dots = FALSE)
    cols <- get_across_cols(data, call$cols, {{ .by }})
    cols <- syms(cols)
    call2("vec_c", !!!cols, .ns = "vctrs")
  } else if (is_call(x, c("if_all.", "if_any."))) {
    call <- match.call(tidytable::if_all., x, expand.dots = FALSE)
    if (is.null(call$.fns)) return(TRUE)
    .cols <- get_across_cols(data, call$.cols, {{ .by }})
    call_list <- map.(.cols, ~ fn_to_expr(call$.fns, .x))
    reduce_fn <- if (is_call(x, "if_all.")) "&" else "|"
    filter_expr <- call_reduce(call_list, reduce_fn)
    prep_expr(filter_expr, data, {{ .by }})
  } else if (is_call(x, "across.")) {
    call <- match.call(tidytable::across., x, expand.dots = FALSE)
    .cols <- get_across_cols(data, call$.cols, {{ .by }})
    dots <- call$...
    call_list <- across_calls(call$.fns, .cols, call$.names, dots)
    prep_exprs(call_list, data, {{ .by }})
  } else {
    x[-1] <- lapply(x[-1], prep_expr, data, {{ .by }})
    x
  }
}

internal_if_else <- function(condition, true, false, missing = NULL) {
  abort("Only for use in prep_exprs")
}
