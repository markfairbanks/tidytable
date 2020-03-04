#' Filter rows on one or more conditions
#'
#' @description
#' Filters a dataset to choose rows where conditions are true.
#'
#' @param .data A data.frame or data.table
#' @param ... Conditions to filter by
#'
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   a = c(1,2,3),
#'   b = c(4,5,6))
#'
#' example_dt %>%
#'   dt_filter(a >= 2, b >= 4)
dt_filter <- function(.data, ...) {
  UseMethod("dt_filter")
}

#' @export
dt_filter.tidytable <- function(.data, ...) {

  dots <- enexprs(...)

  for (dot in dots) {
    .data <- eval_tidy(expr(
      .data[!!dot]
    ))
  }
  .data
}

#' @export
dt_filter.data.frame <- function(.data, ...) {
  .data <- as_tidytable(.data)

  dt_filter(.data, ...)
}
