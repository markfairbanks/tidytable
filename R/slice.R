#' Choose rows in a data.table
#'
#' Choose rows in a data.table.
#' Grouped data.tables grab rows within each group.
#'
#' @param .df A data.frame or data.table
#' @param ... Integer row values
#' @param order_by Variable to arrange by
#' @param n Number of rows to grab
#' @param .by Columns to group by
#'
#' @export
#'
#' @examples
#' test_df <- data.table(
#'   x = c(1,2,3,4),
#'   y = c(4,5,6,7),
#'   z = c("a","a","a","b"))
#'
#' test_df %>%
#'   slice.(1:3)
#'
#' test_df %>%
#'   slice.(1, 3)
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
slice. <- function(.df, ..., .by = NULL) {
  UseMethod("slice.")
}

#' @export
slice..data.frame <- function(.df, ..., .by = NULL) {

  .df <- as_tidytable(.df)

  rows <- enquos(...) # Needed so .N works

  if (length(rows) == 0) return(.df)

  data_env <- env(quo_get_env(rows[[1]]), .df = .df)

  .by <- enquo(.by)

  by_is_null <- quo_is_null(.by)

  if (by_is_null) {
    eval_quo(
      .df[{.rows = c(!!!rows); .rows[data.table::between(.rows, -.N, .N)]}],
      new_data_mask(data_env), env = caller_env()
    )
  } else {
    .df_names <- names(.df)

    .by <- select_vec_chr(.df, !!.by)

    slice_call <- quo(
      {.rows = c(!!!rows);
      .rows = .rows[data.table::between(.rows, -.N, .N)];
      vctrs::vec_slice(.SD, .rows)}
    )

    if (all(.df_names %in% .by)) {
      eval_quo(
        .df[, !!slice_call, by = !!.by, .SDcols = !!.df_names][, (!!.df_names) := NULL][],
        new_data_mask(data_env), env = caller_env()
      )
    } else {
      .df <- eval_quo(
        .df[, !!slice_call, by = !!.by],
        new_data_mask(data_env), env = caller_env()
      )
      # Need to preserve original column order
      setcolorder(.df, .df_names)[]
    }
  }
}

#' @export
#' @rdname slice.
slice_head. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_head.")
}

#' @export
slice_head..data.frame <- function(.df, n = 5, .by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  data_env <- env(quo_get_env(n), .df = .df)

  .by <- select_vec_chr(.df, {{ .by }})

  with_by <- length(.by) > 0

  .df_names <- names(.df)

  if (all(.df_names %in% .by)) {
    eval_quo(
      .df[, head(.SD, !!n), by = !!.by, .SDcols = !!.df_names][, (!!.df_names) := NULL][],
      new_data_mask(data_env), env = caller_env()
    )
  } else {
    .df <- eval_quo(
      .df[, head(.SD, !!n), by = !!.by],
      new_data_mask(data_env), env = caller_env()
    )
    if (with_by) setcolorder(.df, .df_names)[]
    else .df
  }

}

#' @export
#' @rdname slice.
slice_tail. <- function(.df, n = 5, .by = NULL) {
  UseMethod("slice_tail.")
}

#' @export
slice_tail..data.frame <- function(.df, n = 5, .by = NULL) {

  .df <- as_tidytable(.df)

  n <- enquo(n)

  data_env <- env(quo_get_env(n), .df = .df)

  .by <- select_vec_chr(.df, {{ .by }})

  with_by <- length(.by) > 0

  .df_names <- names(.df)

  if (all(.df_names %in% .by)) {
    eval_quo(
      .df[, tail(.SD, !!n), by = !!.by, .SDcols = !!.df_names][, (!!.df_names) := NULL][],
      new_data_mask(data_env), env = caller_env()
    )
  } else {
    .df <- eval_quo(
      .df[, tail(.SD, !!n), by = !!.by],
      new_data_mask(data_env), env = caller_env()
    )
    if (with_by) setcolorder(.df, .df_names)[]
    else .df
  }
}

#' @export
#' @rdname slice.
slice_max. <- function(.df, order_by, n = 1, .by = NULL) {
  UseMethod("slice_max.")
}

#' @export
slice_max..data.frame <- function(.df, order_by, n = 1, .by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .df %>%
    arrange.(-{{ order_by }}) %>%
    slice_head.(n, .by = {{ .by }})
}

#' @export
#' @rdname slice.
slice_min. <- function(.df, order_by, n = 1, .by = NULL) {
  UseMethod("slice_min.")
}

#' @export
slice_min..data.frame <- function(.df, order_by, n = 1, .by = NULL) {

  .df <- as_tidytable(.df)

  if (missing(order_by)) stop("order_by must be supplied")

  .df %>%
    arrange.({{ order_by }}) %>%
    slice_head.(n, .by = {{ .by }})
}


