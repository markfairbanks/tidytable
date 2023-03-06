#' Arrange/reorder rows
#'
#' @description
#' Order rows in ascending or descending order.
#'
#' @param .df A data.frame or data.table
#' @param ... Variables to arrange by
#'
#' @export
#'
#' @examples
#' df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a", "a", "b")
#' )
#'
#' df %>%
#'   arrange(c, -a)
#'
#' df %>%
#'   arrange(c, desc(a))
arrange <- function(.df, ...) {
  .df <- .df_as_tidytable(.df)

  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  dots <- prep_exprs(dots, .df, dt_env = dt_env)

  is_expr <- map_lgl(dots, ~ !is_symbol(.x) && !is_call(.x, "-", 1))

  if (any(is_expr)) {
    i <- expr(order(!!!dots))

    dt_expr <- call2_i(.df, i)
  } else {
    .df <- copy(.df)

    dt_expr <- call2("setorder", quo(.df), !!!dots, na.last = TRUE, .ns = "data.table")

    dt_expr <- call2("[", dt_expr)
  }

  eval_tidy(dt_expr, .df, dt_env)
}

#' @export
#' @keywords internal
#' @inherit arrange
arrange. <- function(.df, ...) {
  deprecate_dot_fun()
  arrange(.df, ...)
}
