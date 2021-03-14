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
arrange. <- function(.df, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.df, ...) {
  .df <- as_tidytable(.df)

  dots <- enquos(...)

  if (length(dots) == 0) return(.df)

  dots <- map.(dots, clean_expr, .df)

  i <- expr(order(!!!dots))

  dt_expr <- dt_call_i(.df, i)

  eval_tidy(dt_expr)
}
