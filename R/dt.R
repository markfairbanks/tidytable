#' Pipeable data.table call
#'
#' @description
#' Pipeable data.table call
#'
#' Note: This function does not use data.table's modify-by-reference
#'
#' @param .data A data.frame or data.table
#' @param ... Arguments passed to data.table call. See ?data.table:::`[.data.table`
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3),
#'   y = c(4,5,6),
#'   z = c("a", "a", "b"))
#'
#' example_dt %>%
#'   dt(, ':='(double_x = x * 2)) %>%
#'   dt(order(-double_x))
#' @export
dt <- function(.data, ...) {
  UseMethod("dt")
}

#' @export
dt.data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)
  dt(.data, ...)
}

#' @export
dt.tidytable <- function(.data, ...) {
  .data[...]
}
