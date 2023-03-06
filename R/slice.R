#' Choose rows in a data.table
#'
#' Choose rows in a data.table.
#' Grouped data.tables grab rows within each group.
#'
#' @param .df A data.frame or data.table
#' @param ... Integer row values
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param .by,by Columns to group by
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
#'   slice(1:3)
#'
#' df %>%
#'   slice(1, 3)
#'
#' df %>%
#'   slice(1:2, .by = z)
#'
#' df %>%
#'   slice_head(1, .by = z)
#'
#' df %>%
#'   slice_tail(1, .by = z)
#'
#' df %>%
#'   slice_max(order_by = x, .by = z)
#'
#' df %>%
#'   slice_min(order_by = y, .by = z)
slice <- function(.df, ..., .by = NULL) {
  .df <- .df_as_tidytable(.df)

  if (is_ungrouped(.df)) {
    tt_slice(.df, ..., .by = {{ .by }})
  } else {
    .by <- group_vars(.df)
    tt_slice(.df, ..., .by = any_of(.by))
  }
}

#' @export
#' @keywords internal
#' @inherit slice
slice. <- function(.df, ..., .by = NULL) {
  deprecate_dot_fun()
  slice(.df, ..., .by = {{ .by }})
}

tt_slice <- function(.df, ..., .by = NULL) {
  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  dots <- prep_exprs(dots)

  .by <- tidyselect_names(.df, {{ .by }})

  i <- expr(vctrs::num_as_location(c(!!!dots), .N, oob = "remove"))

  dt_expr <- call2_i(.df, i, .by)

  eval_tidy(dt_expr, env = dt_env)
}

