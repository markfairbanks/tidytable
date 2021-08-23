#' Choose rows in a data.table
#'
#' Choose rows in a data.table.
#' Grouped data.tables grab rows within each group.
#'
#' @param .df A data.frame or data.table
#' @param ... Integer row values
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param .by Columns to group by
#' @param prop The proportion of rows to select
#' @param weight_by Sampling weights
#' @param replace Should sampling be performed with (`TRUE`) or without (`FALSE`, default) replacement
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = 1:4,
#'   y = 5:8,
#'   z = c("a", "a", "a", "b")
#' )
#'
#' test_df %>%
#'   slice.(1:3)
#'
#' test_df %>%
#'   slice.(1, 3)
#'
#' test_df %>%
#'   slice.(1:2, .by = z)
#'
#' test_df %>%
#'   slice_head.(1, .by = z)
#'
#' test_df %>%
#'   slice_tail.(1, .by = z)
#'
#' test_df %>%
#'   slice_max.(order_by = x, .by = z)
#'
#' test_df %>%
#'   slice_min.(order_by = y, .by = z)
slice. <- function(.df, ..., .by = NULL) {
  UseMethod("slice.")
}

#' @export
slice..tidytable <- function(.df, ..., .by = NULL) {
  dots <- enquos(...)
  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  dots <- prep_exprs(dots)

  .by <- enquo(.by)

  by_is_null <- quo_is_null(.by)

  if (by_is_null) {
    i <- expr({.rows = c(!!!dots); .rows[data.table::between(.rows, -.N, .N)]})
    dt_expr <- call2_i(.df, i)
  } else {
    .by <- tidyselect_names(.df, !!.by)

    j <- expr(
      {.rows = c(!!!dots);
      .rows = .rows[data.table::between(.rows, -.N, .N)];
      .I[.rows]}
    )

    dt_expr <- call2_fast_by_i(.df, j, .by)
  }

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice..data.frame <- function(.df, ..., .by = NULL) {
  .df <- as_tidytable(.df)
  slice.(.df, ..., .by = {{ .by }})
}

#' @export
#' @rdname slice.
slice_head. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_head.")
}

#' @export
slice_head..tidytable <- function(.df, n = 5, .by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, {{ .by }})

  j <- expr(.I[seq.int(min(!!n, .N))])

  dt_expr <- call2_fast_by_i(.df, j, .by)

  .df <- eval_tidy(dt_expr, env = dt_env)

  .df
}

#' @export
slice_head..data.frame <- function(.df, n = 5, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_head.(.df, {{ n }}, {{ .by }})
}

#' @export
#' @rdname slice.
slice_tail. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_tail.")
}

#' @export
slice_tail..tidytable <- function(.df, n = 5, .by = NULL) {
  n <- enquo(n)

  dt_env <- get_dt_env(n)

  n <- prep_expr(n)

  .by <- tidyselect_names(.df, {{ .by }})

  j <- expr(.I[seq.int(.N - min(!!n, .N) + 1, .N)])

  dt_expr <- call2_fast_by_i(.df, j, .by)

  .df <- eval_tidy(dt_expr, env = dt_env)

  .df
}

#' @export
slice_tail..data.frame <- function(.df, n = 5, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_tail.(.df, {{ n }}, {{ .by }})
}

#' @export
#' @rdname slice.
slice_max. <- function(.df, order_by, n = 1, .by = NULL) {
  UseMethod("slice_max.")
}

#' @export
slice_max..tidytable <- function(.df, order_by, n = 1, .by = NULL) {
  if (missing(order_by)) abort("order_by must be supplied")

  .df %>%
    arrange.(-{{ order_by }}) %>%
    slice_head.(n, .by = {{ .by }})
}

#' @export
slice_max..data.frame <- function(.df, order_by, n = 1, .by = NULL) {
  .df <- as_tidytable(.df)

  if (missing(order_by)) abort("order_by must be supplied")

  slice_max.(.df, {{ order_by }}, n = 1, .by = {{ .by }})
}

#' @export
#' @rdname slice.
slice_min. <- function(.df, order_by, n = 1, .by = NULL) {
  UseMethod("slice_min.")
}

#' @export
slice_min..tidytable <- function(.df, order_by, n = 1, .by = NULL) {
  if (missing(order_by)) abort("order_by must be supplied")

  .df %>%
    arrange.({{ order_by }}) %>%
    slice_head.(n, .by = {{ .by }})
}

#' @export
slice_min..data.frame <- function(.df, order_by, n = 1, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_min.(.df, {{ order_by }}, n = 1, .by = {{ .by }})
}

#' @export
#' @rdname slice.
slice_sample. <- function(.df, n, prop, weight_by = NULL,
                          replace = FALSE, .by = NULL) {
  UseMethod("slice_sample.")
}

#' @export
slice_sample..tidytable <- function(.df, n, prop, weight_by = NULL,
                                     replace = FALSE, .by = NULL) {
  size <- check_slice_size(n, prop, "slice_sample")

  idx <- switch(size$type,
                n =    function(x, n) sample_int(n, size$n, replace = replace, wt = x),
                prop = function(x, n) sample_int(n, size$prop * n, replace = replace, wt = x),
  )

  slice.(.df, idx({{ weight_by }}, .N), .by = {{ .by }})
}

#' @export
slice_sample..data.frame <- function(.df, n, prop, weight_by = NULL,
                                     replace = FALSE, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_sample.(
    .df, n, prop, {{ weight_by }}, replace, {{ .by }}
  )
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (replace) {
    sample.int(n, size, prob = wt, replace = TRUE)
  } else {
    sample.int(n, min(size, n), prob = wt)
  }
}

check_constant <- function(x, name, fn) {
  withCallingHandlers(force(x), error = function(e) {
    abort(c(
      glue("`{name}` must be a constant in `{fn}()`."),
      x = conditionMessage(e)
    ), parent = e)
  })
}

check_slice_size <- function(n, prop, .slice_fn = "check_slice_size") {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    n <- check_constant(n, "n", .slice_fn)
    if (!is.numeric(n) || length(n) != 1) {
      abort("`n` must be a single number.")
    }
    if (is.na(n) || n < 0) {
      abort("`n` must be a non-missing positive number.")
    }

    list(type = "n", n = n)
  } else if (!missing(prop) && missing(n)) {
    prop <- check_constant(prop, "prop", .slice_fn)
    if (!is.numeric(prop) || length(prop) != 1) {
      abort("`prop` must be a single number")
    }
    if (is.na(prop) || prop < 0) {
      abort("`prop` must be a non-missing positive number.")
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply exactly one of `n` and `prop` arguments.")
  }
}

globalVariables("V1")
