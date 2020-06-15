#' Choose rows by position
#'
#' Choose rows by their ordinal position in a data.table. Grouped data.tables use the ordinal position within the group.
#'
#' @param .df A data.frame or data.table
#' @param rows Integer row values. Provide either positive values to keep, or negative values to drop. The values provided must be either all positive or all negative.
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param .by Columns to group by
#' @param by This argument has been renamed to .by and is deprecated
#'
#' @export
#' @md
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,2,3,4),
#'   y = c(4,5,6,7),
#'   z = c("a","a","a","b"))
#'
#' test_df %>%
#'   slice.(1:4)
#'
#' test_df %>%
#'   slice.(1, .by = z)
#'
#' test_df %>%
#'   slice_head.(1, .by = z)
#'
#' test_df %>%
#'   slice_tail.(1, .by = z)
#'
#' test_df %>%
#'   slice_max.(order_by = x, .by = z)
#'
#' test_df %>%
#'   slice_min.(order_by = y, .by = z)
slice. <- function(.df, rows = 1:5, .by = NULL, by = NULL) {
  UseMethod("slice.")
}

#' @export
slice..data.frame <- function(.df, rows = 1:5, .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  rows <- enquo(rows) # Needed so 1:.N works

  .by <- check_dot_by(enquo(.by), enquo(by), "slice.")
  .by <- select_vec_chr(.df, !!.by)

  if (length(.by) == 0) {
    eval_quo(
      .df[1:.N %between% c(min(!!rows), max(!!rows))]
    )
  } else {
    eval_quo(
      .df[, .SD[1:.N %between% c(min(!!rows), max(!!rows))], by = .by]
    )
  }
}

#' @export
#' @rdname slice.
slice_head. <- function(.df, n = 5, .by = NULL, by = NULL) {
  UseMethod("slice_head.")
}

#' @export
slice_head..data.frame <- function(.df, n = 5, .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  .by <- check_dot_by(enquo(.by), enquo(by), "slice_head.")
  .by <- select_vec_chr(.df, !!.by)

  eval_quo(
    .df[, head(.SD, !!n), by = !!.by]
  )
}

#' @export
#' @rdname slice.
slice_tail. <- function(.df, n = 5, .by = NULL, by = NULL) {
  UseMethod("slice_tail.")
}

#' @export
slice_tail..data.frame <- function(.df, n = 5, .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  .by <- check_dot_by(enquo(.by), enquo(by), "slice_tail.")
  .by <- select_vec_chr(.df, !!.by)

  eval_quo(
    .df[, tail(.SD, !!n), by = .by]
  )
}

#' @export
#' @rdname slice.
slice_max. <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {
  UseMethod("slice_max.")
}

#' @export
slice_max..data.frame <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .by <- check_dot_by(enquo(.by), enquo(by), "slice_max.")

  .df %>%
    arrange.(-{{ order_by }}) %>%
    slice_head.({{ n }}, .by = !!.by)
}

#' @export
#' @rdname slice.
slice_min. <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {
  UseMethod("slice_min.")
}

#' @export
slice_min..data.frame <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .by <- check_dot_by(enquo(.by), enquo(by), "slice_min.")

  .df %>%
    arrange.({{ order_by }}) %>%
    slice_head.({{ n }}, .by = !!.by)
}

#' @export
#' @rdname slice.
dt_slice <- function(.df, rows = 1:5, .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_slice()", "slice.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  slice.(.df, {{ rows }}, .by = !!.by)
}

#' @export
#' @rdname slice.
dt_slice_head <- function(.df, n = 5, .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_slice_head()", "slice_head.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  slice_head.(.df, {{ n }}, .by = !!.by)
}

#' @export
#' @rdname slice.
dt_slice_tail <- function(.df, n = 5, .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_slice_tail()", "slice_tail.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  slice_tail.(.df, {{ n }}, .by = !!.by)
}

#' @export
#' @rdname slice.
dt_slice_min <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_slice_min()", "slice_min.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  slice_min.(.df, order_by = {{ order_by }}, n = {{ n }}, .by = !!.by)
}


#' @export
#' @rdname slice.
dt_slice_max <- function(.df, order_by, n = 1, .by = NULL, by = NULL) {
  deprecate_soft("0.5.2", "tidytable::dt_slice_max()", "slice_max.()")

  .by <- check_dot_by(enquo(.by), enquo(by))

  slice_max.(.df, order_by = {{ order_by }}, n = {{ n }}, .by = !!.by)
}




