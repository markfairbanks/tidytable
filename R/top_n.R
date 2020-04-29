#' Select top (or bottom) n rows (by value)
#'
#' @description
#' Select the top or bottom entries in each group, ordered by `wt`.
#'
#' @param .data A data.frame or data.table
#' @param n Number of rows to return
#' @param wt Optional. The variable to use for ordering. If NULL uses the last column in the data.table.
#' @param by Columns to group by
#'
#' @md
#' @export
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = 1:5,
#'   y = 6:10,
#'   z = c(rep("a", 3), rep("b", 2)))
#'
#' example_dt %>%
#'   top_n.(2, wt = y)
#'
#' example_dt %>%
#'   top_n.(2, wt = y, by = z)
top_n. <- function(.data, n = 5, wt = NULL, by = NULL) {
  UseMethod("top_n.")
}

#' @export
top_n..data.frame <- function(.data, n = 5, wt = NULL, by = NULL) {

  .data <- as_tidytable(.data)
  n <- enexpr(n)
  wt <- enexpr(wt)
  by <- enexpr(by)

  if (is.null(wt)) {
    slice_head.(.data, !!n, !!by)
  } else {
    slice_max.(.data, order_by = !!wt, n = !!n, by = !!by)
  }
}

#' @export
#' @rdname top_n.
dt_top_n <- top_n.
