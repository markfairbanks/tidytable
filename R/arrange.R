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
#' example_dt <- tidytable(
#'   a = c(1,2,3),
#'   b = c(4,5,6),
#'   c = c("a","a","b"))
#'
#' example_dt %>%
#'   arrange.(c, -a)
#'
#' example_dt %>%
#'   arrange.(c, desc.(a))
arrange. <- function(.data, ...) {
  UseMethod("arrange.")
}

#' @export
arrange..data.frame <- function(.data, ...) {

  .data <- as_tidytable(.data)

  dots <- enexprs(...)

  eval_expr(
    .data[order(!!!dots)]
  )
}

#' @export
#' @rdname arrange.
dt_arrange <- arrange.
