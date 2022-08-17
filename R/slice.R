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
#' @param with_ties Should ties be kept together. The default `TRUE` may return
#'   can return multiple rows if they are equal. Use `FALSE` to ignore ties.
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   x = 1:4,
#'   y = 5:8,
#'   z = c("a", "a", "a", "b")
#' )
#'
#' df %>%
#'   slice.(1:3)
#'
#' df %>%
#'   slice.(1, 3)
#'
#' df %>%
#'   slice.(1:2, .by = z)
#'
#' df %>%
#'   slice_head.(1, .by = z)
#'
#' df %>%
#'   slice_tail.(1, .by = z)
#'
#' df %>%
#'   slice_max.(order_by = x, .by = z)
#'
#' df %>%
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

  .by <- tidyselect_names(.df, {{ .by }})

  i <- expr({.rows = c(!!!dots); .rows[data.table::between(.rows, -.N, .N)]})

  dt_expr <- call2_i(.df, i, .by)

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

  i <- expr(seq.int(min(!!n, .N)))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
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

  i <- expr(seq.int(.N - min(!!n, .N) + 1, .N))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

#' @export
slice_tail..data.frame <- function(.df, n = 5, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_tail.(.df, {{ n }}, {{ .by }})
}

#' @export
#' @rdname slice.
slice_max. <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  check_required(order_by)
  UseMethod("slice_max.")
}

#' @export
slice_max..tidytable <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  if (is_true(with_ties)) {
    .df %>%
      filter.(
        frank({{ order_by }}, ties.method = "max") > (.N - .env$n),
        .by = {{ .by }}
      ) %>%
      arrange.(-{{ order_by }})
  } else {
    .df %>%
      arrange.(-{{ order_by }}) %>%
      slice_head.(n, .by = {{ .by }})
  }
}

#' @export
slice_max..data.frame <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_max.(.df, {{ order_by }}, n = n, with_ties = with_ties, .by = {{ .by }})
}

#' @export
#' @rdname slice.
slice_min. <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  check_required(order_by)
  UseMethod("slice_min.")
}

#' @export
slice_min..tidytable <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  if (is_true(with_ties)) {
    .df %>%
      filter.(
        frank({{ order_by }}, ties.method = "min") <= .env$n,
        .by = {{ .by }}
      ) %>%
      arrange.({{ order_by }})
  } else {
    .df %>%
      arrange.({{ order_by }}) %>%
      slice_head.(n, .by = {{ .by }})
  }
}

#' @export
slice_min..data.frame <- function(.df, order_by, n = 1, ..., with_ties = TRUE, .by = NULL) {
  .df <- as_tidytable(.df)
  slice_min.(.df, {{ order_by }}, n = n, with_ties = with_ties, .by = {{ .by }})
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
  if (missing(n) && missing(prop)) {
    abort("Must supply either `n` or `prop`")
  } else if (missing(prop)) {
    prop <- 1
  } else {
    n <- expr(.N)
  }

  slice.(
    .df,
    sample_int(.N, !!n * !!prop, replace, wt = {{ weight_by }}),
    .by = {{ .by }}
  )
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

globalVariables("V1")
