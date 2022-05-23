#' Arrange/reorder rows
#'
#' @description Order rows in ascending or descending order.
#'
#' Note: `data.table` orders character columns slightly differently than `dplyr::arrange()` by
#' ordering in the "C-locale". See `?data.table::setorder` for more details.
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
#'   arrange.(c, -a)
#'
#' df %>%
#'   arrange.(c, desc(a))
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..tidytable <- function(.df, ...) {
  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  dt_env <- get_dt_env(dots)

  dots <- prep_exprs(dots, .df, dt_env = dt_env)

  out <- copy(.df)

  dt_expr <- call2("setorder", quo(out), !!!dots, .ns = "data.table")

  out <- eval_tidy(dt_expr)

  out
}

#' @export
arrange..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  arrange.(.df, ...)
}
