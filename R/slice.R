#' Choose rows by position
#'
#' Choose rows by their ordinal position in a data.table. Grouped data.tables use the ordinal position within the group.
#'
#' @param .data A data.frame or data.table
#' @param rows Integer row values. Provide either positive values to keep, or negative values to drop. The values provided must be either all positive or all negative.
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param by Columns to group by
#'
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
#'   slice.(1:4)
#'
#' example_dt %>%
#'   slice.(1, by = z)
#'
#' example_dt %>%
#'   slice_head.(1, by = z)
#'
#' example_dt %>%
#'   slice_tail.(1, by = z)
#'
#' example_dt %>%
#'   slice_max.(order_by = x, by = z)
#'
#' example_dt %>%
#'   slice_min.(order_by = y, by = z)
slice. <- function(.data, rows = 1:5, by = NULL) {
  UseMethod("slice.")
}

#' @export
slice..tidytable <- function(.data, rows = 1:5, by = NULL) {

  rows <- enexpr(rows) # Needed so 1:.N works
  by <- enexpr(by)

  if (is.null(by)) {
    eval_expr(
      .data[!!rows]
    )
  } else {
    by <- vec_selector_by(.data, !!by)

    eval_expr(
      .data[, .SD[!!rows], !!by]
    )
  }
}

#' @export
slice..data.frame <- function(.data, rows = 1:5, by = NULL) {

  .data <- as_tidytable(.data)
  rows <- enexpr(rows)
  by <- enexpr(by)

  slice.(.data, rows = !!rows, by = !!by)
}

#' @export
#' @rdname slice.
slice_head. <- function(.data, n = 5, by = NULL) {
  UseMethod("slice_head.")
}

#' @export
slice_head..tidytable <- function(.data, n = 5, by = NULL) {

  n <- enexpr(n)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  eval_expr(
    .data[, head(.SD, !!n), !!by]
  )
}

#' @export
slice_head..data.frame <- function(.data, n = 5, by = NULL) {

  .data <- as_tidytable(.data)
  n <- enexpr(n)
  by <- enexpr(by)

  slice_head.(.data, n = !!n, by = !!by)
}

#' @export
#' @rdname slice.
slice_tail. <- function(.data, n = 5, by = NULL) {
  UseMethod("slice_tail.")
}

#' @export
slice_tail..tidytable <- function(.data, n = 5, by = NULL) {

  n <- enexpr(n)
  by <- enexpr(by)
  by <- vec_selector_by(.data, !!by)

  eval_expr(
    .data[, tail(.SD, !!n), !!by]
  )
}

#' @export
slice_tail..data.frame <- function(.data, n = 5, by = NULL) {

  .data <- as_tidytable(.data)
  n <- enexpr(n)
  by <- enexpr(by)

  slice_tail.(.data, n = !!n, by = !!by)
}

#' @export
#' @rdname slice.
slice_max. <- function(.data, order_by, n = 1, by = NULL) {
  UseMethod("slice_max.")
}

#' @export
slice_max..data.frame <- function(.data, order_by, n = 1, by = NULL) {
  if (!is_tidytable(.data)) .data <- as_tidytable(.data)

  if (missing(order_by)) stop("order_by must be supplied")

  order_by <- enexpr(order_by)
  n <- enexpr(n)
  by <- enexpr(by)

  .data %>%
    arrange.(-!!order_by) %>%
    slice_head.(!!n, by = !!by)
}

#' @export
#' @rdname slice.
slice_min. <- function(.data, order_by, n = 1, by = NULL) {
  UseMethod("slice_min.")
}

#' @export
slice_min..data.frame <- function(.data, order_by, n = 1, by = NULL) {
  if (!is_tidytable(.data)) .data <- as_tidytable(.data)

  if (missing(order_by)) stop("order_by must be supplied")

  order_by <- enexpr(order_by)
  n <- enexpr(n)
  by <- enexpr(by)

  .data %>%
    arrange.(!!order_by) %>%
    slice_head.(!!n, by = !!by)
}

#' @export
#' @rdname slice.
dt_slice <- slice.

#' @export
#' @rdname slice.
dt_slice_head <- slice_head.

#' @export
#' @rdname slice.
dt_slice_tail <- slice_tail.

#' @export
#' @rdname slice.
dt_slice_min <- slice_min.

#' @export
#' @rdname slice.
dt_slice_max <- slice_max.



