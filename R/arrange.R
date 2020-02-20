#' Arrange/reorder rows by variables
#'
#' @description Order rows in ascending or descending order
#'
#' @param .data A data.frame or data.table
#' @param ... Variables to arrange by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   dt_arrange(a, -c)
dt_arrange <- function(.data, ...) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  dots <- enexprs(...)

  eval_tidy(expr(
    .data[order(!!!dots)]
  ))
}
