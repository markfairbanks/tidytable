#' Choose rows by position
#'
#' Choose rows by their ordinal position in a data.table. Grouped data.tables use the ordinal position within the group.
#'
#' @param .data A data.frame or data.table
#' @param rows Integer row values. Provide either positive values to keep, or negative values to drop. The values provided must be either all positive or all negative.
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param by A single unquoted column, or a `list()` of columns to group by.
#'
#' @return data.table
#' @import data.table
#' @export
#' @md
#'
#' @examples
#' example_dt <- data.table::data.table(
#'   x = c(1,2,3,4),
#'   y = c(4,5,6,7),
#'   z = c("a","a","a","b"))
#'
#' example_dt %>%
#'   dt_slice(1:4)
#'
#' example_dt %>%
#'   dt_slice(1, by = z)
#'
#' example_dt %>%
#'   dt_slice_head(1, by = z)
#'
#' example_dt %>%
#'   dt_slice_tail(1, by = z)
#'
#' example_dt %>%
#'   dt_slice_max(order_by = x, by = z)
#'
#' example_dt %>%
#'   dt_slice_min(order_by = y, by = z)
dt_slice <- function(.data, rows = 1:5, by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (!is.numeric(rows)) stop("rows must be a numeric vector")

  rows <- enexpr(rows)
  by <- enexpr(by)

  if (is.null(by)) {
    eval_tidy(expr(
      .data[!!rows]
    ))
  } else {
    eval_tidy(expr(
      .data[, .SD[!!rows], !!by]
    ))
  }
}

#' @export
#' @rdname dt_slice
dt_slice_head <- function(.data, n = 5, by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (!is.numeric(n) | length(n) > 1) stop("n must be a single number")

  by <- enexpr(by)

  eval_tidy(expr(
    .data[, head(.SD, n), !!by]
  ))
}

#' @export
#' @rdname dt_slice
dt_slice_tail <- function(.data, n = 5, by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (!is.numeric(n) | length(n) > 1) stop("n must be a single number")

  by <- enexpr(by)

  eval_tidy(expr(
    .data[, tail(.SD, n), !!by]
  ))
}

#' @export
#' @rdname dt_slice
dt_slice_max <- function(.data, order_by, n = 1, by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (missing(order_by)) stop("order_by must be supplied")

  if (!is.numeric(n) | length(n) > 1) stop("n must be a single number")

  order_by <- enexpr(order_by)
  by <- enexpr(by)

  .data %>%
    dt_arrange(-!!order_by) %>%
    dt_slice_head(n, by = !!by)
}

#' @export
#' @rdname dt_slice
dt_slice_min <- function(.data, order_by, n = 1, by = NULL) {
  if (!is.data.frame(.data)) stop(".data must be a data.frame or data.table")
  if (!is.data.table(.data)) .data <- as.data.table(.data)

  if (missing(order_by)) stop("order_by must be supplied")

  if (!is.numeric(n) | length(n) > 1) stop("n must be a single number")

  order_by <- enexpr(order_by)
  by <- enexpr(by)

  .data %>%
    dt_arrange(!!order_by) %>%
    dt_slice_head(n, by = !!by)
}
