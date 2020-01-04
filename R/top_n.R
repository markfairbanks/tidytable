#' Select top (or bottom) n rows (by value)
#'
#' @description
#' This is a wrapper that uses `dt[order()]` to select the top or bottom entries in each group, ordered by `wt`.
#'
#' @param .data A data.frame or data.table
#' @param n Number of rows to return
#' @param wt (Optional). The variable to use for ordering.
#' @param by `list()` of bare column names to group by
#'
#' @md
#' @return A data.table
#' @export
#'
#' @examples
#' example_dt <- data.table(x = 1:5,
#'                          y = 6:10,
#'                          z = c(rep("a", 3), rep("b", 2)))
#'
#' example_dt %>%
#'   test_top_n(2, wt = y)
#'
#' example_dt %>%
#'   test_top_n(2, wt = y, by = z)
dt_top_n <- function(.data, n = NULL, wt = NULL, by) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (is.null(n)) stop("n must be supplied")
  if (!is.numeric(n) | length(n) > 1) stop("n must be a single number")

  wt <- substitute(wt)

  if (is.null(wt)) {
    wt <- as.symbol(colnames(.data)[length(.data)])

    eval.parent(substitute(
      .data[order(-wt), head(.SD, n), by]
    ))
  } else if (wt >= 0) {
    eval.parent(substitute(
      .data[order(-wt)][, head(.SD, n), by]
    ))
  } else {
    eval.parent(substitute(
      .data[order(wt)][, head(.SD, abs(n)), by]
    ))
  }
}
