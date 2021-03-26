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
#' test_df <- data.table(
#'   a = 1:3,
#'   b = 4:6,
#'   c = c("a","a","b")
#' )
#'
#' test_df %>%
#'   arrange.(c, -a)
#'
#' test_df %>%
#'   arrange.(c, desc(a))
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)
  .df <- copy(.df)

  dots <- enquos(...)
  if (length(dots) == 0) return(.df)
  dots <- prep_exprs(dots, .df)

  dt_expr <- dt_call2("setorder", .df, !!!dots)
  dt_expr <- call2("[", dt_expr)

  eval_tidy(dt_expr)
}
