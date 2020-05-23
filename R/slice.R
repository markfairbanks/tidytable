#' Choose rows by position
#'
#' Choose rows by their ordinal position in a data.table. Grouped data.tables use the ordinal position within the group.
#'
#' @param .df A data.frame or data.table
#' @param rows Integer row values. Provide either positive values to keep, or negative values to drop. The values provided must be either all positive or all negative.
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param by Columns to group by
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
#'   slice.(1, by = z)
#'
#' test_df %>%
#'   slice_head.(1, by = z)
#'
#' test_df %>%
#'   slice_tail.(1, by = z)
#'
#' test_df %>%
#'   slice_max.(order_by = x, by = z)
#'
#' test_df %>%
#'   slice_min.(order_by = y, by = z)
slice. <- function(.df, rows = 1:5, by = NULL) {
  UseMethod("slice.")
}

#' @export
slice..data.frame <- function(.df, rows = 1:5, by = NULL) {

  .df <- as_tidytable(.df)

  rows <- enquo(rows) # Needed so 1:.N works
  by <- select_vec_by(.df, {{ by }})

  eval_quo(
    .df[, eval_quo({.N = .env$.N; .SD[!!rows]}, .SD), !!by]
  )
}

#' @export
#' @rdname slice.
slice_head. <- function(.df, n = 5, by = NULL) {
  UseMethod("slice_head.")
}

#' @export
slice_head..data.frame <- function(.df, n = 5, by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  by <- select_vec_by(.df, {{ by }})

  eval_quo(
    .df[, eval_quo({.N = .env$.N; head(.SD, !!n)}, .SD), !!by]
  )
}

#' @export
#' @rdname slice.
slice_tail. <- function(.df, n = 5, by = NULL) {
  UseMethod("slice_tail.")
}

#' @export
slice_tail..data.frame <- function(.df, n = 5, by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  by <- select_vec_by(.df, {{ by }})

  eval_quo(
    .df[, eval_quo({.N = .env$.N; tail(.SD, !!n)}, .SD), !!by]
  )
}

#' @export
#' @rdname slice.
slice_max. <- function(.df, order_by, n = 1, by = NULL) {
  UseMethod("slice_max.")
}

#' @export
slice_max..data.frame <- function(.df, order_by, n = 1, by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .df %>%
    arrange.(-{{ order_by }}) %>%
    slice_head.(n, by = {{ by }})
}

#' @export
#' @rdname slice.
slice_min. <- function(.df, order_by, n = 1, by = NULL) {
  UseMethod("slice_min.")
}

#' @export
slice_min..data.frame <- function(.df, order_by, n = 1, by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .df %>%
    arrange.({{ order_by }}) %>%
    slice_head.(n, by = {{ by }})
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



